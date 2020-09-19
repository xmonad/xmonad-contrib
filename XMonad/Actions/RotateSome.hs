-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RotateSome
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
    surfaceNext,
    surfacePrev,
    rotateSome,
  ) where

import Control.Arrow ((***))
import Data.List (partition, sortOn, (\\))
import qualified Data.Map as M
import XMonad (Window, X, runLayout, screenRect, windows, withWindowSet)
import XMonad.StackSet (Screen (Screen), Stack (Stack), current, floating, modify', stack)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.RotateSome
--
-- and add keybindings such as the following:
--
-- >   , ((modMask .|. controlMask, xK_n), surfaceNext)
-- >   , ((modMask .|. controlMask, xK_p), surfacePrev)
--

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
  windows . modify' $ reverseStack . rotateSome (`elem` ring) . reverseStack

surfaceRing :: X [Window]
surfaceRing = withWindowSet $ \wset -> do
  let Screen wsp _ sd = current wset

  case stack wsp >>= filter' (`M.notMember` floating wset) of
    Nothing -> pure []
    Just st -> go st . fst <$> runLayout wsp {stack = Just st} (screenRect sd)
  where
    go (Stack t ls rs) recs = t : ((ls ++ rs) \\ map fst recs)

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
    (xs, anchors) =
      partition (p . snd) $
        zip
          [negate (length ls)..]
          (reverse ls ++ t : rs)

    (ls', t':rs') =
      (map snd *** map snd)
        . span ((< 0) . fst)
        . sortOn fst
        . (++) anchors
        . map (fst *** snd)
        $ zip xs (rotate xs)
  in
    Stack t' (reverse ls') rs'

rotate :: [a] -> [a]
rotate = uncurry (flip (++)) . splitAt 1

reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls
