-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicBars
-- Description :  Manage per-screen status bars.
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

module XMonad.Hooks.DynamicBars {-# DEPRECATED "Use XMonad.Hooks.StatusBar instead" #-} (
  -- * Usage
  -- $usage
    DynamicStatusBar
  , DynamicStatusBarCleanup
  , DynamicStatusBarPartialCleanup
  , dynStatusBarStartup
  , dynStatusBarStartup'
  , dynStatusBarEventHook
  , dynStatusBarEventHook'
  , multiPP
  , multiPPFormat
  ) where

import Prelude

import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrandr

import System.IO

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- Provides a few helper functions to manage per-screen status bars while
-- dynamically responding to screen changes. A startup action, event hook, and
-- a way to separate PP styles based on the screen's focus are provided:
--
-- * The 'dynStatusBarStartup' hook which initializes the status bars. The
-- first argument is an `ScreenId -> IO Handle` which spawns a status bar on the
-- given screen and returns the pipe which the string should be written to.
-- The second argument is a `IO ()` to shut down all status bars. This should
-- be placed in your `startupHook`.
--
-- * The 'dynStatusBarEventHook' hook which respawns status bars when the
-- number of screens changes. The arguments are the same as for the
-- `dynStatusBarStartup` function. This should be placed in your
-- `handleEventHook`.
--
-- * Each of the above functions have an alternate form
-- (`dynStatusBarStartup'` and `dynStatusBarEventHook'`) which use a cleanup
-- function which takes an additional `ScreenId` argument which allows for
-- more fine-grained control for shutting down a specific screen's status bar.
--
-- * The 'multiPP' function which allows for different output based on whether
-- the screen for the status bar has focus (the first argument) or not (the
-- second argument). This is for use in your `logHook`.
--
-- * The 'multiPPFormat' function is the same as the 'multiPP' function, but it
-- also takes in a function that can customize the output to status bars.
--
-- The hooks take a 'DynamicStatusBar' function which is given the id of the
-- screen to start up and returns the 'Handle' to the pipe to write to. The
-- 'DynamicStatusBarCleanup' argument should tear down previous instances. It
-- is called when the number of screens changes and on startup.
--

newtype DynStatusBarInfo = DynStatusBarInfo
  { dsbInfo :: [(ScreenId, Handle)]
  }

instance ExtensionClass DynStatusBarInfo where
  initialValue = DynStatusBarInfo []

type DynamicStatusBar = ScreenId -> IO Handle
type DynamicStatusBarCleanup = IO ()
type DynamicStatusBarPartialCleanup = ScreenId -> IO ()

dynStatusBarSetup :: X ()
dynStatusBarSetup = do
  dpy <- asks display
  root <- asks theRoot
  io $ xrrSelectInput dpy root rrScreenChangeNotifyMask

dynStatusBarStartup :: DynamicStatusBar -> DynamicStatusBarCleanup -> X ()
dynStatusBarStartup sb cleanup = do
  dynStatusBarSetup
  updateStatusBars sb cleanup

dynStatusBarStartup' :: DynamicStatusBar -> DynamicStatusBarPartialCleanup -> X ()
dynStatusBarStartup' sb cleanup = do
  dynStatusBarSetup
  updateStatusBars' sb cleanup

dynStatusBarEventHook :: DynamicStatusBar -> DynamicStatusBarCleanup -> Event -> X All
dynStatusBarEventHook sb cleanup = dynStatusBarRun (updateStatusBars sb cleanup)

dynStatusBarEventHook' :: DynamicStatusBar -> DynamicStatusBarPartialCleanup -> Event -> X All
dynStatusBarEventHook' sb cleanup = dynStatusBarRun (updateStatusBars' sb cleanup)

dynStatusBarRun :: X () -> Event -> X All
dynStatusBarRun action RRScreenChangeNotifyEvent{} = action >> return (All True)
dynStatusBarRun _      _                           = return (All True)

updateStatusBars :: DynamicStatusBar -> DynamicStatusBarCleanup -> X ()
updateStatusBars sb cleanup = do
  (dsbInfoScreens, dsbInfoHandles) <- XS.get <&> unzip . dsbInfo
  screens <- getScreens
  when (screens /= dsbInfoScreens) $ do
      newHandles <- liftIO $ do
          hClose `mapM_` dsbInfoHandles
          cleanup
          mapM sb screens
      XS.put $ DynStatusBarInfo (zip screens newHandles)

updateStatusBars' :: DynamicStatusBar -> DynamicStatusBarPartialCleanup -> X ()
updateStatusBars' sb cleanup = do
  (dsbInfoScreens, dsbInfoHandles) <- XS.get <&> (unzip . dsbInfo)
  screens <- getScreens
  when (screens /= dsbInfoScreens) $ do
      let oldInfo = zip dsbInfoScreens dsbInfoHandles
      let (infoToKeep, infoToClose) = partition (flip elem screens . fst) oldInfo
      newInfo <- liftIO $ do
          mapM_ (hClose . snd) infoToClose
          mapM_ (cleanup . fst) infoToClose
          let newScreens = screens \\ dsbInfoScreens
          newHandles <- mapM sb newScreens
          return $ zip newScreens newHandles
      XS.put . DynStatusBarInfo $ infoToKeep ++ newInfo

-----------------------------------------------------------------------------
-- The following code is from adamvo's xmonad.hs file.
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo%27s_xmonad.hs

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> X ()
multiPP = multiPPFormat dynamicLogString

multiPPFormat :: (PP -> X String) -> PP -> PP -> X ()
multiPPFormat dynlStr focusPP unfocusPP = do
  (_, dsbInfoHandles) <- XS.get <&> unzip . dsbInfo
  multiPP' dynlStr focusPP unfocusPP dsbInfoHandles

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
  traverse_ put . getLast
    =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
    =<< mapM screenWorkspace (zipWith const [0 .. ] handles)

getScreens :: MonadIO m => m [ScreenId]
getScreens = liftIO $ do
  screens <- do
    dpy <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  let ids = zip [0 .. ] screens
  return $ map fst ids
