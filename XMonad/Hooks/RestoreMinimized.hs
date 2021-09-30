-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.RestoreMinimized
-- Description :  Deprecated: Use XMonad.Hooks.Minimize.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- (Deprecated: Use XMonad.Hooks.Minimize) Lets you restore minimized
-- windows (see "XMonad.Layout.Minimize") by selecting them on a
-- taskbar (listens for _NET_ACTIVE_WINDOW and WM_CHANGE_STATE).
--
-----------------------------------------------------------------------------

module XMonad.Hooks.RestoreMinimized
    {-# DEPRECATED "Use XMonad.Hooks.Minimize instead, this module has no effect" #-}
    ( -- * Usage
      -- $usage
      RestoreMinimized (..)
    , restoreMinimizedEventHook
    ) where

import XMonad.Prelude

import XMonad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.RestoreMinimized
-- >
-- > myHandleEventHook = restoreMinimizedEventHook
-- >
-- > main = xmonad def { handleEventHook = myHandleEventHook }

data RestoreMinimized = RestoreMinimized deriving ( Show, Read )

restoreMinimizedEventHook :: Event -> X All
restoreMinimizedEventHook _ = return (All True)
