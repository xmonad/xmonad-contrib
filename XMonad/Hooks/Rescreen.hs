-- |
-- Module      :  XMonad.Hooks.Rescreen
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- Custom hooks for screen (xrandr) configuration changes.
--
module XMonad.Hooks.Rescreen (
    -- * Usage
    -- $usage
    rescreenHook,
    rescreenEventHook,
    ) where

import Control.Monad.Fix (fix)
import Control.Monad (when)
import Data.Monoid (All(..))

import XMonad

-- $usage
-- This module provides a replacement for the screen configuration change
-- handling in core that enables attaching a custom hook that can
-- restart/reposition status bars or systray.
--
-- You can use it by including the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.RescreenHook
--
-- defining your custom rescreen hook:
--
-- > myRescreenHook :: X ()
-- > myRescreenHook = …
--
-- and adding 'rescreenHook' to your 'xmonad' config:
--
-- > main = xmonad $ … . rescreenHook myRescreenHook . … $ def{…}

-- | Attach a custom hook when the screen configuration changes (due to
-- xrandr). Replaces the built-in rescreen handling of xmonad core with:
--
-- 1. suppress duplicate change events
-- 2. 'rescreen'
-- 3. invoke specified hook
--
-- Useful for restarting/repositioning status bars and systray.
rescreenHook :: X () -> XConfig a -> XConfig a
rescreenHook hook xConfig =
    xConfig{ handleEventHook = handleEventHook xConfig <> rescreenEventHook hook }

-- | Event hook with custom rescreen hook. See 'rescreenHook' for more.
rescreenEventHook :: X () -> Event -> X All
rescreenEventHook hook ConfigureEvent{ev_event_type = t, ev_window = w} = do
    isRescreen <- isRoot w
    if isRescreen
        then do
            -- Xorg emits several ConfigureEvents after every change,
            -- clear them to prevent triggering the hook multiple times
            clearTypedWindowEvents w t
            rescreen
            hook
            return (All False)
        else mempty
rescreenEventHook _ _ = mempty

-- | Remove all X events of a given window and type from the event queue.
clearTypedWindowEvents :: Window -> EventType -> X ()
clearTypedWindowEvents w t = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \e -> fix $ \again -> do
        more <- checkTypedWindowEvent d w t e
        when more again
