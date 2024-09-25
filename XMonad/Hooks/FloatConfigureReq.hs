{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  XMonad.Hooks.FloatConfigureReq
-- Description :  Customize handling of floating windows' move\/resize\/restack requests (ConfigureRequest).
-- Copyright   :  (c) 2024 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- xmonad normally honours those requests by doing exactly what the client
-- application asked, and refreshing. There are some misbehaving clients,
-- however, that:
--
-- * try to move their window to the last known absolute position regardless
--   of the current xrandr/xinerama layout
--
-- * move their window to 0, 0 for no particular reason (e.g. rxvt-unicode)
--
-- * issue lots of no-op requests causing flickering (e.g. Steam)
--
-- This module provides a replacement handler for 'ConfigureRequestEvent' to
-- work around such misbehaviours.
--
module XMonad.Hooks.FloatConfigureReq (
    -- * Usage
    -- $usage
    MaybeMaybeManageHook,
    floatConfReqHook,

    -- * Known workarounds
    fixSteamFlicker,
    fixSteamFlickerMMMH,
    ) where

import qualified Data.Map.Strict as M
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
-- To use this, include the following in your @xmonad.hs@:
--
-- > import XMonad.Hooks.FloatConfigureReq
-- > import XMonad.Hooks.ManageHelpers
--
-- > myFloatConfReqHook :: MaybeMaybeManageHook
-- > myFloatConfReqHook = composeAll
-- >     [ … ]
--
-- > myEventHook :: Event -> X All
-- > myEventHook = mconcat
-- >     [ …
-- >     , floatConfReqHook myFloatConfReqHook
-- >     , … ]
--
-- > main = xmonad $ …
-- >               $ def{ handleEventHook = myEventHook
-- >                    , … }
--
-- Then fill the @myFloatConfReqHook@ with whatever custom rules you need.
--
-- As an example, the following will prevent rxvt-unicode from moving its
-- (floating) window to 0, 0 after a font change but still ensure its size
-- increment hints are respected:
--
-- > className =? "URxvt" -?> pure <$> doFloat
--
-- Another example that avoids flickering and xmonad slowdowns caused by the
-- Steam client (completely ignore all its requests, none of which are
-- meaningful in the context of a tiling WM):
--
-- > map toLower `fmap` className =? "steam" -?> mempty
--
-- (this example is also available as 'fixSteamFlickerMMMH' to be added to
-- one's @myFloatConfReqHook@ and also 'fixSteamFlicker' to be added directly
-- to one's 'handleEventHook')

-- | A variant of 'MaybeManageHook' that additionally may or may not make
-- changes to the 'WindowSet'.
type MaybeMaybeManageHook = Query (Maybe (Maybe (Endo WindowSet)))

-- | Customizable handler for a 'ConfigureRequestEvent'. If the event's
-- 'ev_window' is a managed floating window, the provided
-- 'MaybeMaybeManageHook' is consulted and its result interpreted as follows:
--
--  * @Nothing@ - no match, fall back to the default handler
--
--  * @Just Nothing@ - match but ignore, no refresh, just send ConfigureNotify
--
--  * @Just (Just a)@ - match, modify 'WindowSet', refresh, send ConfigureNotify
floatConfReqHook :: MaybeMaybeManageHook -> Event -> X All
floatConfReqHook mh ConfigureRequestEvent{ev_window = w} =
    runQuery (join <$> (isFloatQ -?> mh)) w >>= \case
        Nothing -> mempty
        Just e -> do
            whenJust e (windows . appEndo)
            sendConfEvent
            pure (All False)
  where
    sendConfEvent = withDisplay $ \dpy ->
        withWindowAttributes dpy w $ \wa -> do
            io . allocaXEvent $ \ev -> do
                -- We may have made no changes to the window size/position
                -- and thus the X server didn't emit any ConfigureNotify,
                -- so we need to send the ConfigureNotify ourselves to make
                -- sure there is a reply to this ConfigureRequestEvent and the
                -- window knows we (possibly) ignored its request.
                setEventType ev configureNotify
                setConfigureEvent ev w w
                    (wa_x wa) (wa_y wa) (wa_width wa)
                    (wa_height wa) (wa_border_width wa) none (wa_override_redirect wa)
                sendEvent dpy w False 0 ev
floatConfReqHook _ _ = mempty

-- | A 'Query' to determine if a window is floating.
isFloatQ :: Query Bool
isFloatQ = ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset

-- | A pre-packaged 'floatConfReqHook' that fixes flickering of the Steam client by ignoring 'ConfigureRequestEvent's on any of its floating windows.
--
-- To use this, add 'fixSteamFlicker' to your 'handleEventHook'.
fixSteamFlicker :: Event -> X All
fixSteamFlicker = floatConfReqHook fixSteamFlickerMMMH

fixSteamFlickerMMMH :: MaybeMaybeManageHook
fixSteamFlickerMMMH = map toLower `fmap` className =? "steam" -?> mempty
