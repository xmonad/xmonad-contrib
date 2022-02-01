-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Hacks
-- Description :  A collection of small fixes and utilities with possibly hacky implementations.
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module is a collection of random fixes, workarounds and other functions
-- that rely on somewhat hacky implementations which may have unwanted side effects
-- and/or are small enough to not warrant a separate module.
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

  -- * Inform xmobar when trays (e.g. trayer) change width
  -- $padTrayer
  trayerPaddingXmobarEventHook,
  trayPaddingXmobarEventHook,
  trayPaddingEventHook,
  ) where


import XMonad
import XMonad.Hooks.StatusBar (xmonadPropLog')
import XMonad.Prelude (All (All), fi, filterM, when)
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
-- <<https://user-images.githubusercontent.com/79924233/115355075-e61dd280-a1ec-11eb-81d3-927ca462945f.gif>>
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

-- | Like 'trayAbovePanelEventHook', but specialised for trayer/xmobar.
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

-- $padTrayer
-- Communicating tray (e.g., trayer) resize events to xmobar so that
-- padding space may be reserved on xmobar for the tray.
--
-- Basic Usage with trayer:
--
-- First, add the padding hook to your @handleEventHook@ as follows:
--
-- > main = xmonad $ def
-- > { ...
-- > , handleEventHook = handleEventHook def
-- >                  <> Hacks.trayerPaddingXmobarEventHook
-- > }
--
-- Then, assuming the tray is placed on the right, update your
-- @xmobarrc@ as follows:
--
-- > Config { ...
-- >        , commands = [ ...
-- >                     , Run XPropertyLog "_XMONAD_TRAYPAD", ... ]
-- >        , template = " ... %_XMONAD_TRAYPAD%"
-- >        }
--
-- As an example of what happens in this basic usage, consider the
-- case where trayer updates to a width of 53 pixels.
-- The following property will appear on the root window:
--
-- > _XMONAD_TRAYPAD(UTF8_STRING) = "<hspace=53/>"

-- | A simple trayer/xmobar-specific event hook that watches for trayer window
-- resize changes and updates the _XMONAD_TRAYPAD property with xmobar markup
-- that leaves a gap for the trayer.
trayerPaddingXmobarEventHook :: Event -> X All -- ^ event hook
trayerPaddingXmobarEventHook = trayPaddingXmobarEventHook (className =? "trayer") "_XMONAD_TRAYPAD"

-- | A generic version of 'trayerPaddingXmobarEventHook' that
-- allows the user to specify how to identify a tray window and the property
-- to use with 'xmonadPropLog''. This is useful for other trays like
-- stalonetray and also when space for more than one tray-like window needs to
-- be reserved.
trayPaddingXmobarEventHook
  :: Query Bool     -- ^ query to identify the tray window
  -> String         -- ^ 'xmonadPropLog'' property to use
  -> Event -> X All -- ^ resulting event hook
trayPaddingXmobarEventHook trayQ prop = trayPaddingEventHook trayQ hspaceLog
  where hspaceLog width = xmonadPropLog' prop ("<hspace=" ++ show width ++ "/>")

-- | A fully generic tray resize hook that invokes a callback whenever a
-- tray-like window changes width.
trayPaddingEventHook
  :: Query Bool     -- ^ query to identify the tray window
  -> (Int -> X ())  -- ^ action to perform when tray width changes
  -> Event -> X All -- ^ resulting event hook
trayPaddingEventHook trayQ widthChanged ConfigureEvent{ ev_window = w, ev_width = wa } = do
  whenX (runQuery trayQ w) $ widthChanged (fi wa)
  mempty
trayPaddingEventHook _ _ _ = mempty
