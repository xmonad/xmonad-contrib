{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      :  XMonad.Hooks.Rescreen
-- Description :  Custom hooks for screen (xrandr) configuration changes.
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- Custom hooks for screen (xrandr) configuration changes.
--
module XMonad.Hooks.Rescreen (
    -- * Usage
    -- $usage
    addAfterRescreenHook,
    addRandrChangeHook,
    RescreenConfig(..),
    rescreenHook,
    ) where

import Graphics.X11.Xrandr
import XMonad
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleConf as XC

-- $usage
-- This module provides a replacement for the screen configuration change
-- handling in core that enables attaching custom hooks to screen (xrandr)
-- configuration change events. These can be used to restart/reposition status
-- bars or systrays automatically after xrandr
-- ('XMonad.Hooks.StatusBar.dynamicSBs' uses this module internally), as well
-- as to actually invoke xrandr or autorandr when an output is (dis)connected.
--
-- To use this, include the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.Rescreen
--
-- define your custom hooks:
--
-- > myAfterRescreenHook :: X ()
-- > myAfterRescreenHook = spawn "fbsetroot -solid red"
--
-- > myRandrChangeHook :: X ()
-- > myRandrChangeHook = spawn "autorandr --change"
--
-- and hook them into your 'xmonad' config:
--
-- > main = xmonad $ …
-- >               . addAfterRescreenHook myAfterRescreenHook
-- >               . addRandrChangeHook myRandrChangeHook
-- >               . …
-- >               $ def{…}
--
-- See documentation of 'rescreenHook' for details about when these hooks are
-- called.

-- | Hook configuration for 'rescreenHook'.
data RescreenConfig = RescreenConfig
    { afterRescreenHook :: X () -- ^ hook to invoke after 'rescreen'
    , randrChangeHook :: X () -- ^ hook for other randr changes, e.g. (dis)connects
    }

instance Default RescreenConfig where
    def = RescreenConfig
        { afterRescreenHook = mempty
        , randrChangeHook = mempty
        }

instance Semigroup RescreenConfig where
    RescreenConfig arh rch <> RescreenConfig arh' rch' = RescreenConfig (arh <> arh') (rch <> rch')

instance Monoid RescreenConfig where
    mempty = def

-- | Attach custom hooks to screen (xrandr) configuration change events.
-- Replaces the built-in rescreen handling of xmonad core with:
--
-- 1. listen to 'RRScreenChangeNotifyEvent' in addition to 'ConfigureEvent' on
--    the root window
-- 2. whenever such event is received:
-- 3. clear any other similar events (Xorg server emits them in bunches)
-- 4. if any event was 'ConfigureEvent', 'rescreen' and invoke 'afterRescreenHook'
-- 5. if there was no 'ConfigureEvent', invoke 'randrChangeHook' only
--
-- 'afterRescreenHook' is useful for restarting/repositioning status bars and
-- systray.
--
-- 'randrChangeHook' may be used to automatically trigger xrandr (or perhaps
-- autorandr) when outputs are (dis)connected.
--
-- Note that 'rescreenHook' is safe to use several times, 'rescreen' is still
-- done just once and hooks are invoked in sequence, also just once.
rescreenHook :: RescreenConfig -> XConfig l -> XConfig l
rescreenHook = XC.once $ \c -> c
    { startupHook = startupHook c <> rescreenStartupHook
    , handleEventHook = handleEventHook c <> rescreenEventHook }

-- | Shortcut for 'rescreenHook'.
addAfterRescreenHook :: X () -> XConfig l -> XConfig l
addAfterRescreenHook h = rescreenHook def{ afterRescreenHook = userCodeDef () h }

-- | Shortcut for 'rescreenHook'.
addRandrChangeHook :: X () -> XConfig l -> XConfig l
addRandrChangeHook h = rescreenHook def{ randrChangeHook = userCodeDef () h }

-- | Startup hook to listen for @RRScreenChangeNotify@ events.
rescreenStartupHook :: X ()
rescreenStartupHook = do
    dpy <- asks display
    root <- asks theRoot
    io $ xrrSelectInput dpy root rrScreenChangeNotifyMask

-- | Event hook with custom rescreen/randr hooks. See 'rescreenHook' for more.
rescreenEventHook :: Event -> X All
rescreenEventHook e = do
    shouldHandle <- case e of
        ConfigureEvent{ ev_window = w } -> isRoot w
        RRScreenChangeNotifyEvent{ ev_window = w } -> isRoot w
        _ -> pure False
    if shouldHandle
        then All False <$ handleEvent e
        else mempty

handleEvent :: Event -> X ()
handleEvent e = XC.with $ \RescreenConfig{..} -> do
    -- Xorg emits several events after every change, clear them to prevent
    -- triggering the hook multiple times.
    moreConfigureEvents <- clearTypedWindowEvents (ev_window e) configureNotify
    _ <- clearTypedWindowRREvents (ev_window e) rrScreenChangeNotify
    -- If there were any ConfigureEvents, this is an actual screen
    -- configuration change, so rescreen and fire rescreenHook. Otherwise,
    -- this is just a connect/disconnect, fire randrChangeHook.
    if ev_event_type e == configureNotify || moreConfigureEvents
        then rescreen >> afterRescreenHook
        else randrChangeHook

-- | Remove all X events of a given window and type from the event queue,
-- return whether there were any.
clearTypedWindowEvents :: Window -> EventType -> X Bool
clearTypedWindowEvents w t = withDisplay $ \d -> io $ allocaXEvent (go d)
  where
    go d e' = do
        sync d False
        gotEvent <- checkTypedWindowEvent d w t e'
        e <- if gotEvent then Just <$> getEvent e' else pure Nothing
        gotEvent <$ if
            | not gotEvent -> mempty
            | (ev_window <$> e) == Just w -> void $ go d e'
            -- checkTypedWindowEvent checks ev_event instead of ev_window, so
            -- we may need to put some events back
            | otherwise -> allocaXEvent (go d) >> io (putBackEvent d e')

clearTypedWindowRREvents :: Window -> EventType -> X Bool
clearTypedWindowRREvents w t =
    rrEventBase >>= \case
        Just base -> clearTypedWindowEvents w (base + t)
        Nothing -> pure False

rrEventBase :: X (Maybe EventType)
rrEventBase = withDisplay $ \d ->
    fmap (fromIntegral . fst) <$> io (xrrQueryExtension d)
