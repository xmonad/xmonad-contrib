-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Sift
-- Description :  Functions for sifting windows up and down.
-- Copyright   :  (c) 2020 Ivan Brennan <ivanbrennan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ivan Brennan <ivanbrennan@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- Functions for sifting windows up and down. Sifts behave identically to
-- swaps (i.e. 'swapUp' and 'swapDown' from "XMonad.StackSet"), except in
-- the wrapping case: rather than rotating the entire stack by one position
-- like a swap would, a sift causes the windows at either end of the stack
-- to trade positions.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Sift (
    -- * Usage
    -- $usage
    siftUp,
    siftDown,
  ) where

import XMonad.StackSet (Stack (Stack), StackSet, modify')
import XMonad.Util.Stack (reverseS)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.Sift
--
-- and add keybindings such as the following:
--
-- >   , ((modMask .|. shiftMask, xK_j ), windows siftDown)
-- >   , ((modMask .|. shiftMask, xK_k ), windows siftUp  )
--

-- |
-- siftUp, siftDown. Exchange the focused window with its neighbour in
-- the stack ordering, wrapping if we reach the end. Unlike 'swapUp' and
-- 'swapDown', wrapping is handled by trading positions with the window
-- at the other end of the stack.
--
siftUp, siftDown :: StackSet i l a s sd -> StackSet i l a s sd
siftUp   = modify' siftUp'
siftDown = modify' (reverseS . siftUp' . reverseS)

siftUp' :: Stack a -> Stack a
siftUp' (Stack t (l:ls) rs) = Stack t ls (l:rs)
siftUp' (Stack t []     rs) =
  case reverse rs of
    (x:xs) -> Stack t (xs ++ [x]) []
    []     -> Stack t []          []
