{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RotateSome
-- Description :  Rotate some elements around the stack.
-- Copyright   :  (c) 2020 Ivan Brennan <ivanbrennan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ivan Brennan <ivanbrennan@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- Functions for rotating some elements around the stack while keeping others
-- anchored in place. Useful in combination with layouts that dictate window
-- visibility based on stack position, such as "XMonad.Layout.LimitWindows".
--
-----------------------------------------------------------------------------

module XMonad.Actions.RotateSome (
    -- * Usage
    -- $usage
    -- * Example
    -- $example
    surfaceNext,
    surfacePrev,
    rotateSome,
  ) where

import Control.Arrow ((***))
import XMonad.Prelude (NonEmpty(..), notEmpty, partition, sortOn, (\\))
import qualified Data.Map as M
import XMonad (Window, WindowSpace, Rectangle, X, runLayout, screenRect, windows, withWindowSet)
import XMonad.StackSet (Screen (Screen), Stack (Stack), current, floating, modify', stack)
import XMonad.Util.Stack (reverseS)

{- $usage
You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.RotateSome

and add keybindings such as the following:

>   , ((modMask .|. controlMask, xK_n), surfaceNext)
>   , ((modMask .|. controlMask, xK_p), surfacePrev)

-}

{- $example
#Example#

Consider a workspace whose stack contains five windows A B C D E but whose
layout limits how many will actually be shown, showing only the first plus
two additional windows, starting with the third:

>  ┌─────┬─────┐
>  │     │  C  │
>  │  A  ├─────┤
>  │     │  D  │
>  └─────┴─────┘
>
>  A  B  C  D  E
>  _     ____

If C has focus and we'd like to replace it with one of the unshown windows,
'surfaceNext' will move the next unshown window, E, into the focused position:

>  ┌─────┬─────┐                ┌─────┬─────┐
>  │     │ *C* │                │     │ *E* │
>  │  A  ├─────┤ surfaceNext -> │  A  ├─────┤
>  │     │  D  │                │     │  D  │
>  └─────┴─────┘                └─────┴─────┘
>
>  A  B *C* D  E                A  C *E* D  B
>  _     ____                   _     ____

This repositioned windows B C E by treating them as a sequence that can be
rotated through the focused stack position. Windows A and D remain anchored
to their original (visible) positions.

A second call to 'surfaceNext' moves B into focus:

>  ┌─────┬─────┐                ┌─────┬─────┐
>  │     │ *E* │                │     │ *B* │
>  │  A  ├─────┤ surfaceNext -> │  A  ├─────┤
>  │     │  D  │                │     │  D  │
>  └─────┴─────┘                └─────┴─────┘
>
>  A  C *E* D  B                A  E *B* D  C
>  _     ____                   _     ____

A third call would complete the cycle, bringing C back into focus.

-}

-- |
-- Treating the focused window and any unshown windows as a ring that can be
-- rotated through the focused position, surface the next element in the ring.
surfaceNext :: X ()
surfaceNext = do
  ring <- surfaceRing
  windows . modify' $ rotateSome (`elem` ring)

-- | Like 'surfaceNext' in reverse.
surfacePrev :: X ()
surfacePrev = do
  ring <- surfaceRing
  windows . modify' $ reverseS . rotateSome (`elem` ring) . reverseS

-- |
-- Return a list containing the current focus plus any unshown windows. Note
-- that windows are shown if 'runLayout' provides them with a rectangle or if
-- they are floating.
surfaceRing :: X [Window]
surfaceRing = withWindowSet $ \wset -> do
  let Screen wsp _ sd = current wset

  case stack wsp >>= filter' (`M.notMember` floating wset) of
    Nothing -> pure []
    Just st -> go st <$> layoutWindows wsp {stack = Just st} (screenRect sd)
  where
    go :: Stack Window -> [Window] -> [Window]
    go (Stack t ls rs) shown = t : ((ls ++ rs) \\ shown)

    layoutWindows :: WindowSpace -> Rectangle -> X [Window]
    layoutWindows wsp rect = map fst . fst <$> runLayout wsp rect

-- | Like "XMonad.StackSet.filter" but won't move focus.
filter' :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter' p (Stack f ls rs)
  | p f       = Just $ Stack f (filter p ls) (filter p rs)
  | otherwise = Nothing

-- |
-- @'rotateSome' p stack@ treats the elements of @stack@ that satisfy predicate
-- @p@ as a ring that can be rotated, while all other elements remain anchored
-- in place.
rotateSome :: (a -> Bool) -> Stack a -> Stack a
rotateSome p (Stack t ls rs) =
  let
    -- Flatten the stack, index each element relative to the focused position,
    -- then partition into movable and anchored elements.
    (movables, anchors) =
      partition (p . snd) $
        zip
          [negate (length ls)..]
          (reverse ls ++ t : rs)

    -- Pair each movable element with the index of its next movable neighbor.
    -- Append anchored elements, along with their unchanged indices, and sort
    -- by index. Separate lefts (negative indices) from the rest, and grab the
    -- new focus from the head of the remaining elements.
    (ls', notEmpty -> t' :| rs') =
      (map snd *** map snd)
        . span ((< 0) . fst)
        . sortOn fst
        . (++) anchors
        $ zipWith (curry (fst *** snd)) movables (rotate movables)
  in
    Stack t' (reverse ls') rs'

rotate :: [a] -> [a]
rotate = uncurry (flip (++)) . splitAt 1
