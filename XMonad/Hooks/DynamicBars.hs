-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicBars
-- Copyright   :  (c) Ben Boeckel 2012
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  mathstuf@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Manage per-screen status bars.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicBars (
  -- * Usage
  -- $usage
    DynamicStatusBar
  , DynamicStatusBarCleanup
  , dynStatusBarStartup
  , dynStatusBarEventHook
  , multiPP
  ) where

import Prelude

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)

import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrandr

import System.IO
import System.IO.Unsafe

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog

-- $usage
-- Provides a few helper functions to manage per-screen status bars while
-- dynamically responding to screen changes. A startup action, event hook, and
-- a way to separate PP styles based on the screen's focus are provided:
--
-- * The 'dynStatusBarStartup' hook which initializes the status bars.
--
-- * The 'dynStatusBarEventHook' hook which respawns status bars when the
-- number of screens changes.
--
-- * The 'multiPP' function which allows for different output based on whether
-- the screen for the status bar has focus.
--
-- The hooks take a 'DynamicStatusBar' function which is given the id of the
-- screen to start up and returns the 'Handle' to the pipe to write to. The
-- 'DynamicStatusBarCleanup' argument should tear down previous instances. It
-- is called when the number of screens changes and on startup.
--

data DynStatusBarInfo = DynStatusBarInfo
  { dsbInfoScreens :: [ScreenId]
  , dsbInfoHandles :: [Handle]
  }

type DynamicStatusBar = ScreenId -> IO Handle
type DynamicStatusBarCleanup = IO ()

-- Global state
statusBarInfo :: MVar DynStatusBarInfo
statusBarInfo = unsafePerformIO $ newMVar (DynStatusBarInfo [] [])

dynStatusBarStartup :: DynamicStatusBar -> DynamicStatusBarCleanup -> X ()
dynStatusBarStartup sb cleanup = liftIO $ do
  dpy <- openDisplay ""
  xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
  closeDisplay dpy
  updateStatusBars sb cleanup

dynStatusBarEventHook :: DynamicStatusBar -> DynamicStatusBarCleanup -> Event -> X All
dynStatusBarEventHook sb cleanup (RRScreenChangeNotifyEvent {}) = liftIO (updateStatusBars sb cleanup) >> return (All True)
dynStatusBarEventHook _  _       _                              = return (All True)

updateStatusBars :: DynamicStatusBar -> DynamicStatusBarCleanup -> IO ()
updateStatusBars sb cleanup = liftIO $ do
  dsbInfo <- takeMVar statusBarInfo
  screens <- getScreens
  if (screens /= (dsbInfoScreens dsbInfo))
    then do
      mapM hClose (dsbInfoHandles dsbInfo)
      cleanup
      newHandles <- mapM sb screens
      putMVar statusBarInfo (DynStatusBarInfo screens newHandles)
    else putMVar statusBarInfo dsbInfo

-----------------------------------------------------------------------------
-- The following code is from adamvo's xmonad.hs file.
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo%27s_xmonad.hs

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> X ()
multiPP focusPP unfocusPP = do
  dsbInfo <- liftIO $ readMVar statusBarInfo
  multiPP' dynamicLogString focusPP unfocusPP (dsbInfoHandles dsbInfo)

multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
  st <- get
  let pickPP :: WorkspaceId -> WriterT (Last XState) X String
      pickPP ws = do
        let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset st
        put st{ windowset = W.view ws $ windowset st }
        out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
        when isFoc $ get >>= tell . Last . Just
        return out
  traverse put . getLast
    =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
    =<< mapM screenWorkspace (zipWith const [0 .. ] handles)
  return ()

getScreens :: IO [ScreenId]
getScreens = do
  screens <- do
    dpy <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  let ids = zip [0 .. ] screens
  return $ map fst ids
