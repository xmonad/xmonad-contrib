-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.UpdateFocus
-- Description :  Updates the focus on mouse move in unfocused windows.
-- Copyright   :  (c) Daniel Schoepe
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <asgaroth_@gmx.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Updates the focus on mouse move in unfocused windows.
--
-----------------------------------------------------------------------------

module XMonad.Actions.UpdateFocus (
    -- * Usage
    -- $usage
    focusOnMouseMove,
    adjustEventInput,
    focusUnderPointer,
) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
-- To make the focus update on mouse movement within an unfocused window, add the
-- following to your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.UpdateFocus
-- > xmonad $ def {
-- >   ..
-- >   startupHook = adjustEventInput
-- >   handleEventHook = focusOnMouseMove
-- >   ..
-- > }
--
-- This module is probably only useful when focusFollowsMouse is set to True(default).

-- | Changes the focus if the mouse is moved within an unfocused window.
focusOnMouseMove :: Event -> X All
focusOnMouseMove MotionEvent{ ev_x = x, ev_y = y, ev_window = root } = do
    -- check only every 15 px to avoid excessive calls to translateCoordinates
    when (x `mod` 15 == 0 || y `mod` 15 == 0) $ do
      dpy <- asks display
      foc <- withWindowSet $ return . W.peek
      -- get the window under the pointer:
      (_,_,_,w) <- io $ translateCoordinates dpy root root (fromIntegral x) (fromIntegral y)
      when (foc /= Just w) $ focus w
    return (All True)
focusOnMouseMove _ = return (All True)

-- | Adjusts the event mask to pick up pointer movements.
adjustEventInput :: X ()
adjustEventInput = withDisplay $ \dpy -> do
  rootw <- asks theRoot
  io $ selectInput dpy rootw $  substructureRedirectMask .|. substructureNotifyMask
                                .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
                                .|. buttonPressMask .|. pointerMotionMask

-- | Focus the window under the mouse pointer, unless we're currently changing
-- focus with the mouse or dragging. This is the inverse to
-- "XMonad.Actions.UpdatePointer": instead of moving the mouse pointer to
-- match the focus, we change the focus to match the mouse pointer.
--
-- This is meant to be used together with
-- 'XMonad.Actions.UpdatePointer.updatePointer' in individual key bindings.
-- Bindings that change focus should invoke
-- 'XMonad.Actions.UpdatePointer.updatePointer' at the end, bindings that
-- switch workspaces or change layouts should call 'focusUnderPointer' at the
-- end. Neither should go to 'logHook', as that would override the other.
--
-- This is more finicky to set up than 'focusOnMouseMove', but ensures that
-- focus is updated immediately, without having to touch the mouse.
focusUnderPointer :: X ()
focusUnderPointer = whenX (not <$> (asks mouseFocused <||> gets (isJust . dragging))) $ do
  dpy <- asks display
  root <- asks theRoot
  (_, _, w', _, _, _, _, _) <- io $ queryPointer dpy root
  w <- gets (W.peek . windowset)
  when (w' /= none && Just w' /= w) (focus w')
