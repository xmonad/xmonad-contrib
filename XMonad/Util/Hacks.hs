-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Hacks
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module is a collection of random fixes, workarounds and other functions
-- that rely on somewhat hacky implementations which may have unwanted side effects.
--
-- Import this module as qualified like so:
--
-- > import qualified XMonad.Util.Hacks as Hacks
--
-- and then use the functions you want as described in their respective documentation.
--
-----------------------------------------------------------------------------

module XMonad.Util.Hacks (
  -- * Windowed fullscreen
  -- $windowedFullscreenFix
  windowedFullscreenFixEventHook,

  -- * Java Hack
  -- $java
  javaHack,

  -- * Stacking trays (trayer) above panels (xmobar)
  -- $raiseTrayer
  trayerAboveXmobarEventHook,
  trayAbovePanelEventHook,
  ) where


import XMonad
import Data.Monoid (All(All))
import Control.Monad (when, filterM)
import System.Posix.Env (putEnv)


-- $windowedFullscreenFix
-- Windowed fullscreen describes the behaviour in which XMonad,
-- by default, does not automatically put windows that request being fullscreened
-- into actual fullscreen, but keeps them constrained
-- to their normal window dimensions, still rendering them in fullscreen.
--
-- With chromium based applications like Chrome, Discord and others this
-- can cause issues, where the window does not correctly see the size of the window
-- when displaying the fullscreen content, thus cutting off the window content.
--
-- This function works around that issue by forcing the window to recalculate their
-- dimensions after initiating fullscreen, thus making chrome-based applications
-- behave properly when in windowed fullscreen.
--
-- The following gif shows the behaviour of chrome (left) without this fix
-- compared to firefox, which already behaves as expected by default:
-- <<https://user-images.githubusercontent.com/5300871/99186015-b9512500-274d-11eb-9cfc-6ba3bae6db51.gif>>
--
-- Using this function, chrome will now behave as expected as well:
-- <<https://user-images.githubusercontent.com/5300871/99186115-4dbb8780-274e-11eb-9ed2-b7815ba9e597.gif>>
--
-- Usage:
-- add to handleEventHook as follows:
--
-- > handleEventHook = handleEventHook def <+> Hacks.windowedFullscreenFixEventHook
--

-- | Fixes fullscreen behaviour of chromium based apps by quickly applying and undoing a resize.
-- This causes chromium to recalculate the fullscreen window
-- dimensions to match the actual "windowed fullscreen" dimensions.
windowedFullscreenFixEventHook :: Event -> X All
windowedFullscreenFixEventHook (ClientMessageEvent _ _ _ dpy win typ (_:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
  when (typ == wmstate && fromIntegral fullscreen `elem` dats) $
    withWindowAttributes dpy win $ \attrs ->
      liftIO $ do
        resizeWindow dpy win (fromIntegral $ wa_width attrs - 1) (fromIntegral $ wa_height attrs)
        resizeWindow dpy win (fromIntegral $ wa_width attrs) (fromIntegral $ wa_height attrs)
  return $ All True
windowedFullscreenFixEventHook _ = return $ All True


-- $java
-- Some java Applications might not work with xmonad. A common workaround would be to set the environment
-- variable @_JAVA_AWT_WM_NONREPARENTING@ to 1. The function 'javaHack' does exactly that.
-- Example usage:
--
-- > main = xmonad $ Hacks.javaHack (def {...})
--

-- | Fixes Java applications that don't work well with xmonad, by setting @_JAVA_AWT_WM_NONREPARENTING=1@
javaHack :: XConfig l -> XConfig l
javaHack conf = conf
  { startupHook = startupHook conf
                    *> io (putEnv "_JAVA_AWT_WM_NONREPARENTING=1")
  }


-- $raiseTrayer
-- Placing @trayer@ on top of @xmobar@ is somewhat tricky:
--
-- - they both should be lowered to the bottom of the stacking order to avoid
--   overlapping fullscreen windows
--
-- - the tray needs to be stacked on top of the panel regardless of which
--   happens to start first
--
-- 'trayerAboveXmobarEventHook' (and the more generic
-- 'trayAbovePanelEventHook') is an event hook that ensures the latter:
-- whenever the tray lowers itself to the bottom of the stack, it checks
-- whether there are any panels above it and lowers these again.
--
-- To ensure the former, that is having both @trayer@ and @xmobar@ lower
-- themselves, which is a necessary prerequisite for this event hook to
-- trigger:
--
-- - set @lowerOnStart = True@ and @overrideRedirect = True@ in @~/.xmobarrc@
-- - pass @-l@ to @trayer@
--
-- Usage:
--
-- > handleEventHook = … <> Hacks.trayerAboveXmobarEventHook

-- | 'trayAbovePanelEventHook' for trayer/xmobar
trayerAboveXmobarEventHook :: Event -> X All
trayerAboveXmobarEventHook = trayAbovePanelEventHook (className =? "trayer") (appName =? "xmobar")

-- | Whenever a tray window lowers itself to the bottom of the stack, look for
-- any panels above it and lower these.
trayAbovePanelEventHook
  :: Query Bool -- ^ tray
  -> Query Bool -- ^ panel
  -> (Event -> X All) -- ^ event hook
trayAbovePanelEventHook trayQ panelQ ConfigureEvent{ev_window = w, ev_above = a} | a == none = do
  whenX (runQuery trayQ w) $ withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_, _, ws) <- io $ queryTree dpy rootw
    let aboveTrayWs = dropWhile (w /=) ws
    panelWs <- filterM (runQuery panelQ) aboveTrayWs
    mapM_ (io . lowerWindow dpy) panelWs
  mempty
trayAbovePanelEventHook _ _ _ = mempty
