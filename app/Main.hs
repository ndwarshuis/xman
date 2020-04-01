{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Xcape MANager (XMan) - a wrapper for managing xcape
--
-- xcape is a program to map keyrelease events to keysyms, and is very useful
-- for making custom keymaps. However, it is not always desirable to have this
-- running all the time; for example, VirtualBox will blend the xkb keymap with
-- that if the Guest OS, so xcape may end up producing an extra keypress. The
-- solution is to turn off xcape when certain windows are in focus.
--
-- The process for doing this using Xlib:
-- 1) Listen for PropertyNotify events from the root window
-- 2) Of those events, filter those where the _NET_ACTIVE_WINDOW atom has changed
-- 3) Using the value of _NET_ACTIVE_WINDOW, get the title of the active window
-- 4) If active window matches a certain criteria, turn off xcape (vice versa)
--
-- The matching criteria in (4) are POSIX regular expressions.

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

--------------------------------------------------------------------------------
-- | Central State+Reader+IO Monad (I wonder where this idea came from...)
--
-- The Reader portion holds some of the key data structures from X that we care
-- about as well as the regular expression patterns to match and the keys to
-- pass the xcape command.
--
-- The State portion holds the xcape process handle (so we can kill it later)
-- and the current window title.
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

main :: IO ()
main = getArgs >>= parse

-- | Given a list of arguments, either start the program or print the usage
parse :: [String] -> IO ()
parse [_]    = usage
parse (x:rs) = initXMan x rs
parse _      = usage

-- | Print the usage and exit
usage :: IO ()
-- TODO produce relevant exit codes here
usage = putStrLn "xman XCAPE_KEYS REGEXP [[REGEXP]...]"

-- | Given a string of keys for xcape and a list of regular expressions to match
-- the window titles we care about, initialize the XMan monad and run the main
-- event loop.
initXMan :: String -> [String] -> IO ()
initXMan x r = do
  -- ignore SIGCHLD so we don't produce zombie processes
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
  -- listen only for PropertyNotify events on the root window
  allocaSetWindowAttributes $ \a -> do
    set_event_mask a propertyChangeMask
    changeWindowAttributes dpy root cWEventMask a
  void $ allocaXEvent $ \e ->
    runXMan cf st $ do
      updateXCape -- set the initial state before entering main loop
      forever $ handle =<< io (nextEvent dpy e >> getEvent e)

-- | Lift an IO monad into the XMan context
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Given an initial state and configuration, run the XMan monad
runXMan :: XMConf -> XMState -> XMan a -> IO (a, XMState)
runXMan c s (XMan a) = runStateT (runReaderT a c) s

-- | Update the xcape status given the state of XMan
updateXCape :: XMan ()
updateXCape = do
  dpy <- asks display
  atom <- asks netActiveWindow
  root <- asks theRoot
  prop <- io $ getWindowProperty32 dpy atom root
  case prop of
    Just [aw] -> getTitle aw >>= updateTitle >> startOrKillXCape
    _         -> return ()

-- | Given an event, call a handler. In this case the only thing we care about
-- are PropertyNotify events where the atom is _NET_ACTIVE_WINDOW, which will
-- initiated the xcape update logic.
handle :: Event -> XMan ()
handle PropertyEvent { ev_atom = a } = do
  atom <- asks netActiveWindow
  when (a == atom) updateXCape
handle _ = return ()

-- | Get the title of a window given a window ID
-- The window title could be in two atoms. This will try _NET_WM_NAME first
-- before trying WM_NAME (legacy)
getTitle :: CLong -> XMan (Maybe WindowTitle)
getTitle winID = do
  nwn <- asks netWMName
  doMaybe [nwn, wM_NAME] $ getTitle' winID
  where
    doMaybe (x:xs) f = f x >>= (\r -> if isJust r then return r else doMaybe xs f)
    doMaybe [] _ = return Nothing

-- | Get the title of a window given a window ID and an atom that may contain
-- the title
getTitle' :: CLong -> Atom -> XMan (Maybe WindowTitle)
getTitle' winID atom = do
  dpy <- asks display
  title' <- io $ permitBadWindow $ getWindowProperty8 dpy atom
    $ fromIntegral winID
  return $ fmap (fmap castCCharToChar) title'

-- | Given an IO action (which is assumed to call an XLib function that may
-- throw an error), attach an error handler before performing the action and
-- remove it after it completes. The error handler will ignore BadWindow errors
-- (which in this case are assumed to be benign since the _NET_ACTIVE_WINDOW
-- atom may refer to a non-existent window)
permitBadWindow :: IO a -> IO a
permitBadWindow action = do
  handler <- mkXErrorHandler $ \_ e ->
    getErrorEvent e >>= handleError >> return 0
  original <- _xSetErrorHandler handler
  res <- action
  void $ _xSetErrorHandler original
  return res
  where
    -- TODO also ignore badvalue errors?
    handleError ErrorEvent { ev_error_code = t }
      | fromIntegral t == badWindow = return ()
    handleError _ = print "actual error"

-- | Given a window title that may exist, update the window title in XMan's
-- state
updateTitle :: Maybe WindowTitle -> XMan ()
updateTitle newTitle = modify (\s -> s { currentTitle = newTitle } )

-- | Start or stop xcape given the state in XMan
startOrKillXCape :: XMan ()
startOrKillXCape = do
  -- TODO this is redundant
  title <- gets currentTitle
  case title of
    Just t -> asks regexps >>= \r ->
      if any (t =~) r then stopXCape else startXCape
    Nothing -> startXCape

-- | Start xcape if it is not already running
startXCape :: XMan ()
startXCape = do
  pID <- gets xcapeProcess
  unless (isJust pID) $ do
    x <- asks xcapeKeys
    h <- io $ runXcape x
    modify $ \s -> s { xcapeProcess = Just h }
    io $ print "started xcape"

-- | Stop xcape if it is running
stopXCape :: XMan ()
stopXCape = do
  pID <- gets xcapeProcess
  forM_ pID $ \p -> do
    io $ terminateProcess p
    modify $ \s -> s { xcapeProcess = Nothing }
    io $ print "stopped xcape"

-- | Given the keys argument for xcape, run xcape with the keys argument and
-- return the process handle. Run xcape in debug mode (this will make it run as
-- a foreground process, otherwise it will fork unnecessarily) and pipe the
-- output and error streams to the null device.
-- NOTE: use the process module here rather than the unix module. The latter
-- has the 'forkProcess' function which may fail if multiple instances of xcape
-- are started and killed in quick succession (Resource unavailable error).
runXcape :: String -> IO ProcessHandle
runXcape keys = do
  dn <- fmap UseHandle $ fdToHandle
    =<< openFd "/dev/null" ReadOnly Nothing defaultFileFlags
  -- TODO pass more arguments here? this hardcodes the timeout
  let cp = proc "xcape" $ ["-d", "-t", "500", "-e"] ++ [keys]
  (_, _, _, h) <- createProcess $ cp { std_err = dn, std_out = dn }
  return h
