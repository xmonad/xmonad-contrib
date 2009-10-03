----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.RestoreMinimized
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Lets you restore minimized windows (see "XMonad.Layout.Minimize")
-- by selecting them on a taskbar (listens for _NET_ACTIVE_WINDOW
-- and WM_CHANGE_STATE).
--
-----------------------------------------------------------------------------

module XMonad.Hooks.RestoreMinimized
    ( -- * Usage
      -- $usage
      RestoreMinimized (..)
    , restoreMinimizedEventHook
    ) where

import Data.Monoid
import Control.Monad(when)

import XMonad
import XMonad.Layout.Minimize

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.RestoreMinimized
-- >
-- > myHandleEventHook = restoreMinimizedEventHook
-- >
-- > main = xmonad defaultConfig { handleEventHook = myHandleEventHook }

data RestoreMinimized = RestoreMinimized deriving ( Show, Read )

restoreMinimizedEventHook :: Event -> X All
restoreMinimizedEventHook (ClientMessageEvent {ev_window = w,
                                                ev_message_type = mt}) = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    a_cs <- getAtom "WM_CHANGE_STATE"
    when (mt == a_aw || mt == a_cs) $ do
        sendMessage (RestoreMinimizedWin w)
    return (All True)
restoreMinimizedEventHook _ = return (All True)
