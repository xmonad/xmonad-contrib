----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.Minimize
-- Description :  Handle window manager hints to minimize and restore windows.
-- Copyright   :  (c) Justin Bogner 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Justin Bogner <mail@justinbogner.com>
-- Stability   :  unstable
-- Portability :  not portable
--
-- Handles window manager hints to minimize and restore windows. Use
-- this with "XMonad.Layout.Minimize".
--
-----------------------------------------------------------------------------

module XMonad.Hooks.Minimize
    ( -- * Usage
      -- $usage
      minimizeEventHook
    ) where

import XMonad
import XMonad.Actions.Minimize
import XMonad.Prelude

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.Minimize
-- > import XMonad.Layout.Minimize
-- >
-- > myHandleEventHook = minimizeEventHook
-- > myLayout = minimize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout
-- >                   , handleEventHook = myHandleEventHook }

minimizeEventHook :: Event -> X All
minimizeEventHook ClientMessageEvent{ev_window = w,
                                     ev_message_type = mt,
                                     ev_data = dt} = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    a_cs <- getAtom "WM_CHANGE_STATE"

    when (mt == a_aw) $ maximizeWindow w
    when (mt == a_cs) $ do
      let message = fromIntegral . head $ dt
      when (message == normalState) $ maximizeWindow w
      when (message == iconicState) $ minimizeWindow w

    return (All True)
minimizeEventHook _ = return (All True)
