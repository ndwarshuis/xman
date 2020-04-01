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
-- 3) Using the value of _NET_ACTIVE_WINDOW, get the app name of the active window
-- 4) If the app name matches a certain criteria, turn off xcape (vice versa)
--
-- The matching criteria in (4) are POSIX regular expressions.
--
-- Known limitations:
-- this is agnostic to any keymap changes, so if the keymap is changed, xcape
-- will not be updated or restarted. Furthermore, it is outside the scope of
-- this program to bind multiple xcape mappings with multiple keymaps

module Main where

import           Control.Monad             (forM_, forever, void, when)
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List                 (any)
import           Data.Maybe                (isJust)

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

--------------------------------------------------------------------------------
-- | Central State+Reader+IO Monad (I wonder where this idea came from...)
--
-- The Reader portion holds some of the key data structures from X that we care
-- about as well as the regular expression patterns to match the app names we
-- care about and and the bindings to pass to the xcape command.
--
-- The State portion holds the xcape process handle (so we can kill it later)
newtype XMan a = XMan (ReaderT XMConf (StateT XMState IO) a) deriving
  (Functor, Monad, MonadIO, MonadState XMState, MonadReader XMConf)

instance Applicative XMan where
  pure = return
  (<*>) = ap

newtype XMState = XMState { xcapeHandle :: Maybe ProcessHandle }

data XMConf = XMConf
    { display         :: Display
    , theRoot         :: Window
    , netActiveWindow :: Atom
    , regexps         :: Patterns
    , xcapeProcess    :: CreateProcess
    }

-- | timeout for xcape
type Timeout = Maybe String

-- | bindings for xcape
type Bindings = String

-- | regular expression patterns
type Patterns = [String]

-- | window app name
type AppName = String

--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= parse

-- | Given a list of arguments, either start the program or print the usage
parse :: [String] -> IO ()
parse [_]           = usage
parse ("-t":t:b:rs) = initXMan rs $ mkXcapeProcess (Just t) b
parse (b:rs)        = initXMan rs $ mkXcapeProcess Nothing b
parse _             = usage

-- | Given a timeout and bindings for xcape, return a process record. This will
-- run xcape in debug mode (which will make it run as a foreground process,
-- otherwise it will fork unnecessarily).
mkXcapeProcess :: Timeout -> Bindings -> CreateProcess
mkXcapeProcess (Just t) b =  proc "xcape" $ ["-t", t, "-d", "-e"] ++ [b]
mkXcapeProcess Nothing b  =  proc "xcape" $ ["-d", "-e"] ++ [b]

-- | Print the usage and exit
usage :: IO ()
usage = putStrLn "xman [-t TIMEOUT] BINDINGS REGEXP [[REGEXP] ...]"

-- | Given xcape bindings and regular expression patterns to match the window
-- titles we care about, initialize the XMan monad and run the main event loop
initXMan :: Patterns -> CreateProcess -> IO ()
initXMan rs cp = do
  -- ignore SIGCHLD so we don't produce zombie processes
  void $ installHandler sigCHLD Ignore Nothing
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  naw <- internAtom dpy "_NET_ACTIVE_WINDOW" False
  let cf = XMConf
             { display = dpy
             , theRoot = root
             , netActiveWindow = naw
             , regexps = rs
             , xcapeProcess = cp
             }
      st = XMState { xcapeHandle = Nothing }
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
    Just [aw] -> getAppName (fromIntegral aw) >>= startOrKillXCape
    _         -> return ()

-- | Given an event, call a handler. In this case the only thing we care about
-- are PropertyNotify events where the atom is _NET_ACTIVE_WINDOW, which will
-- initiated the xcape update logic.
handle :: Event -> XMan ()
handle PropertyEvent { ev_atom = a } = do
  atom <- asks netActiveWindow
  when (a == atom) updateXCape
handle _ = return ()

-- | Given a window, return its app name
getAppName :: Window -> XMan AppName
getAppName w = asks display >>= io . fmap resName . flip getClassHint w

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

-- | Given an app name, start or stop xcape if it matches any of the supplied
-- regular expressions in XMan
startOrKillXCape :: AppName -> XMan ()
startOrKillXCape name = do
  rs <- asks regexps
  if any (name =~) rs then stopXCape else startXCape

-- | Start xcape if it is not already running
startXCape :: XMan ()
startXCape = do
  pID <- gets xcapeHandle
  unless (isJust pID) $ do
    cp <- asks xcapeProcess
    h <- io $ createProcessNull cp
    modify $ \s -> s { xcapeHandle = Just h }
    io $ print "started xcape"

-- | Stop xcape if it is running
stopXCape :: XMan ()
stopXCape = do
  pID <- gets xcapeHandle
  forM_ pID $ \p -> do
    io $ terminateProcess p
    modify $ \s -> s { xcapeHandle = Nothing }
    io $ print "stopped xcape"

-- | Given a createProcess record, start the process with stderr and stdout
-- redirected to the null device
-- NOTE: use the process module here rather than the unix module. The latter has
-- the 'forkProcess' function which may fail if multiple instances of xcape are
-- started and killed in quick succession (Resource unavailable error).
createProcessNull :: CreateProcess -> IO ProcessHandle
createProcessNull cp = do
  fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
  dn <- UseHandle <$> fdToHandle fd
  (_, _, _, h) <- createProcess $ cp { std_err = dn, std_out = dn }
  return h
