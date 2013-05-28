-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.UpdateFocus
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
    adjustEventInput
) where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad (when)
import Data.Monoid

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
focusOnMouseMove (MotionEvent { ev_x = x, ev_y = y, ev_window = root }) = do
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
