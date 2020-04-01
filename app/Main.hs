{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad             (forM_, forever, void, when)
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List                 (any)
import           Data.Maybe                (isJust)

import           Foreign.C.String          (castCCharToChar)
import           Foreign.C.Types           (CLong)

import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras
import           Graphics.X11.Xlib.Misc
import           Graphics.X11.Xlib.Types

import           Text.Regex.TDFA

import           System.Environment
import           System.Posix.IO
import           System.Posix.Signals
import           System.Process

type WindowTitle = String

-- I wonder where this idea came from...
newtype XMan a = XMan (ReaderT XMConf (StateT XMState IO) a) deriving
  (Functor, Monad, MonadIO, MonadState XMState, MonadReader XMConf)

instance Applicative XMan where
  pure = return
  (<*>) = ap

data XMState = XMState
    { currentTitle :: Maybe WindowTitle
    , xcapeProcess :: Maybe ProcessHandle
    }

data XMConf = XMConf
    { display         :: Display
    , theRoot         :: Window
    , netActiveWindow :: Atom
    , netWMName       :: Atom
    , regexps         :: [String]
    , xcapeKeys       :: String
    }

io :: MonadIO m => IO a -> m a
io = liftIO

runXMan :: XMConf -> XMState -> XMan a -> IO (a, XMState)
runXMan c s (XMan a) = runStateT (runReaderT a c) s

parse :: [String] -> IO ()
parse [_]    = usage
parse (x:rs) = initXMan x rs
parse _      = usage

usage :: IO ()
usage = putStrLn "xman XCAPE_KEYS REGEXP [[REGEXP]...]"

main :: IO ()
main = getArgs >>= parse

initXMan :: String -> [String] -> IO ()
initXMan x r = do
  void $ installHandler sigCHLD Ignore Nothing
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  naw <- internAtom dpy "_NET_ACTIVE_WINDOW" False
  nwn <- internAtom dpy "_NET_WM_NAME" False
  let cf = XMConf
             { display = dpy
             , theRoot = root
             , netActiveWindow = naw
             , netWMName = nwn
             , regexps = r
             , xcapeKeys = x
             }
      st = XMState { currentTitle = Nothing, xcapeProcess = Nothing }
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a propertyChangeMask
    changeWindowAttributes dpy root cWEventMask a
  void $ allocaXEvent $ \e ->
    runXMan cf st $ do
      updateXCape
      forever $ handle =<< io (nextEvent dpy e >> getEvent e)

updateXCape :: XMan ()
updateXCape = do
  dpy <- asks display
  atom <- asks netActiveWindow
  root <- asks theRoot
  prop <- io $ getWindowProperty32 dpy atom root
  case prop of
    Just [aw] -> getTitle aw >>= updateTitle >> startOrKillXCape
    _         -> return ()

handle :: Event -> XMan ()
handle PropertyEvent { ev_atom = a } = do
  atom <- asks netActiveWindow
  when (a == atom) updateXCape
handle _ = return ()

getTitle :: CLong -> XMan (Maybe WindowTitle)
getTitle winID = do
  nwn <- asks netWMName
  -- try getting _NET_WM_NAME first before trying legacy WM_NAME
  doMaybe [nwn, wM_NAME] $ getTitle' winID
  where
    doMaybe (x:xs) f = f x >>= (\r -> if isJust r then return r else doMaybe xs f)
    doMaybe [] _ = return Nothing

getTitle' :: CLong -> Atom -> XMan (Maybe WindowTitle)
getTitle' winID atom = do
  dpy <- asks display
  title' <- io $ permitBadWindow $ getWindowProperty8 dpy atom
    $ fromIntegral winID
  return $ fmap (fmap castCCharToChar) title'

permitBadWindow :: IO a -> IO a
permitBadWindow action = do
  handler <- mkXErrorHandler $ \_ e ->
    getErrorEvent e >>= handleError >> return 0
  original <- _xSetErrorHandler handler
  res <- action
  void $ _xSetErrorHandler original
  return res
  where
    -- totally ignore BadWindow errors
    -- TODO also ignore badvalue errors?
    handleError ErrorEvent { ev_error_code = t }
      | fromIntegral t == badWindow = return ()
    -- anything not a BadWindow is an unexpected error
    handleError _ = print "actual error"

updateTitle :: Maybe WindowTitle -> XMan ()
updateTitle newTitle = modify (\s -> s { currentTitle = newTitle } )

startOrKillXCape :: XMan ()
startOrKillXCape = do
  title <- gets currentTitle
  case title of
    Just t -> asks regexps >>= \r ->
      if any (t =~) r then stopXCape else startXCape
    Nothing -> startXCape

startXCape :: XMan ()
startXCape = do
  pID <- gets xcapeProcess
  unless (isJust pID) $ do
    x <- asks xcapeKeys
    h <- io $ runXcape x
    modify $ \s -> s { xcapeProcess = Just h }
    io $ print "started xcape"

stopXCape :: XMan ()
stopXCape = do
  pID <- gets xcapeProcess
  forM_ pID $ \p -> do
    io $ terminateProcess p
    modify $ \s -> s { xcapeProcess = Nothing }
    io $ print "stopped xcape"

runXcape :: String -> IO ProcessHandle
runXcape keys = do
  dn <- fmap UseHandle $ fdToHandle
    =<< openFd "/dev/null" ReadOnly Nothing defaultFileFlags
  let cp = proc "xcape" $ ["-d", "-t", "500", "-e"] ++ [keys]
  (_, _, _, h) <- createProcess $ cp { std_err = dn, std_out = dn }
  return h
