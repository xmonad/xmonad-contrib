-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ICCCMFocus
-- Description  : Deprecated.
-- License      : BSD
--
-- Maintainer   : Tony Morris <haskell@tmorris.net>
--
-- Implemented in your @logHook@, Java swing applications will not misbehave
-- when it comes to taking and losing focus.
--
-- This has been done by taking the patch in <http://code.google.com/p/xmonad/issues/detail?id=177> and refactoring it so that it can be included in @~\/.xmonad\/xmonad.hs@.
--
-- @
--    conf' =
--      conf {
--        logHook = takeTopFocus
--      }
-- @
-----------------------------------------------------------------------------
module XMonad.Hooks.ICCCMFocus
{-# DEPRECATED "XMonad.Hooks.ICCCMFocus: xmonad>0.10 core merged issue 177" #-}
(
  atom_WM_TAKE_FOCUS
, takeFocusX
, takeTopFocus
) where

import XMonad
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W

takeFocusX ::
  Window
  -> X ()
takeFocusX _w = return ()

-- | The value to add to your log hook configuration.
takeTopFocus ::
  X ()
takeTopFocus =
  withWindowSet (maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek) >> setWMName "LG3D"
