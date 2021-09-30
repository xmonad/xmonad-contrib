{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Dwindle
-- Description :  Various spirally layouts.
-- Copyright   :  (c) Norbert Zeh <norbert.zeh@gmail.com>
-- License     :  BSD3
--
-- Maintainer  :  Norbert Zeh <norbert.zeh@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Three layouts: The first, 'Spiral', is a reimplementation of
-- 'XMonad.Layout.Spiral.spiral' with, at least to me, more intuitive semantics.
-- The second, 'Dwindle', is inspired by a similar layout in awesome and
-- produces the same sequence of decreasing window sizes as Spiral but pushes
-- the smallest windows into a screen corner rather than the centre.  The third,
-- 'Squeeze' arranges all windows in one row or in one column, with
-- geometrically decreasing sizes.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Dwindle ( -- * Usage
                               -- $usage
                               Dwindle(..)
                             , Direction2D(..)
                             , Chirality(..)
                             ) where

import XMonad.Prelude ( unfoldr )
import XMonad
import XMonad.StackSet ( integrate, Stack )
import XMonad.Util.Types ( Direction2D(..) )

-- $usage
-- This module can be used as follows:
--
-- > import XMonad.Layout.Dwindle
--
-- Then add something like this to your layouts:
--
-- > Dwindle R CW 1.5 1.1
--
-- or
--
-- > Spiral L CW 1.5 1.1
--
-- or
--
-- ^ Squeeze D 1.5 1.1
--
-- The first produces a layout that places the second window to the right of
-- the first, the third below the second, the fourth to the right of the third,
-- and so on.  The first window is 1.5 times as wide as the second one, the
-- second is 1.5 times as tall as the third one, and so on.  Thus, the further
-- down the window stack a window is, the smaller it is and the more it is
-- pushed into the bottom-right corner.
--
-- The second produces a layout with the same window sizes but places the second
-- window to the left of the first one, the third above the second one, the
-- fourth to the right of the third one, and so on.
--
-- The third produces a layout that stacks windows vertically top-down with each
-- window being 1.5 times as tall as the next.
--
-- In all three cases, the fourth (third, in the case of 'Squeeze') parameter,
-- 1.1, is the factor by which the third parameter increases or decreases in
-- response to Expand or Shrink messages.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Layouts with geometrically decreasing window sizes.  'Spiral' and 'Dwindle'
-- split the screen into a rectangle for the first window and a rectangle for
-- the remaining windows, which is split recursively to lay out these windows.
-- Both layouts alternate between horizontal and vertical splits.
--
-- In each recursive step, the split 'Direction2D' determines the placement of the
-- remaining windows relative to the current window: to the left, to the right,
-- above or below.  The split direction of the first split is determined by the
-- first layout parameter.  The split direction of the second step is rotated 90
-- degrees relative to the first split direction according to the second layout
-- parameter of type 'Chirality'.  So, if the first split is 'R' and the second
-- layout parameter is 'CW', then the second split is 'D'.
--
-- For the 'Spiral' layout, the same 'Chirality' is used for computing the split
-- direction of each step from the split direction of the previous step.  For
-- example, parameters 'R' and 'CW' produces the direction sequence 'R', 'D',
-- 'L', 'U', 'R', 'D', 'L', 'U', ...
--
-- For the 'Dwindle' layout, the 'Chirality' alternates between 'CW' and 'CCW' in
-- each step.  For example, parameters 'U' and 'CCW' produce the direction
-- sequence 'U', 'L', 'U', 'L', ... because 'L' is the 'CCW' rotation of 'U' and
-- 'U' is the 'CW' rotation of 'L'.
--
-- In each split, the current rectangle is split so that the ratio between the
-- size of the rectangle allocated to the current window and the size of the
-- rectangle allocated to the remaining windows is the third layout parameter.
-- This ratio can be altered using 'Expand' and 'Shrink' messages.  The former
-- multiplies the ratio by the fourth layout parameter.  The latter divides the
-- ratio by this parameter.
--
-- 'Squeeze' does not alternate between horizontal and vertical splits and
-- simply splits in the direction given as its first argument.
--
-- Parameters for both 'Dwindle' and 'Spiral':
--
-- * First split direction
--
-- * First split chirality
--
-- * Size ratio between rectangle allocated to current window and rectangle
-- allocated to remaining windows
--
-- * Factor by which the size ratio is changed in response to 'Expand' or 'Shrink'
-- messages
--
-- The parameters for 'Squeeze' are the same, except that there is no 'Chirality'
-- parameter.
data Dwindle a = Dwindle !Direction2D !Chirality !Rational !Rational
               | Spiral  !Direction2D !Chirality !Rational !Rational
               | Squeeze !Direction2D !Rational !Rational
               deriving (Read, Show)

-- | Rotation between consecutive split directions
data Chirality = CW | CCW
               deriving (Read, Show)

instance LayoutClass Dwindle a where
    pureLayout (Dwindle dir rot ratio _) = dwindle alternate dir rot ratio
    pureLayout (Spiral  dir rot ratio _) = dwindle rotate    dir rot ratio
    pureLayout (Squeeze dir     ratio _) = squeeze           dir     ratio
    pureMessage (Dwindle dir rot ratio delta) =
      fmap (\ratio' -> Dwindle dir rot ratio' delta) . changeRatio ratio delta
    pureMessage (Spiral dir rot ratio delta) =
      fmap (\ratio' -> Spiral dir rot ratio' delta) . changeRatio ratio delta
    pureMessage (Squeeze dir ratio delta) =
      fmap (\ratio' -> Squeeze dir ratio' delta) . changeRatio ratio delta

changeRatio :: Rational -> Rational -> SomeMessage -> Maybe Rational
changeRatio ratio delta = fmap f . fromMessage
  where f Expand = ratio * delta
        f Shrink = ratio / delta

dwindle :: AxesGenerator -> Direction2D -> Chirality -> Rational -> Rectangle -> Stack a ->
           [(a, Rectangle)]
dwindle trans dir rot ratio rect st = unfoldr genRects (integrate st, rect, dirAxes dir, rot)
  where genRects ([],   _, _, _ ) = Nothing
        genRects ([w],  r, a, rt) = Just ((w, r),  ([], r,   a,  rt))
        genRects (w:ws, r, a, rt) = Just ((w, r'), (ws, r'', a', rt'))
          where (r', r'') = splitRect r ratio a
                (a', rt') = trans a rt

squeeze :: Direction2D -> Rational -> Rectangle -> Stack a -> [(a, Rectangle)]
squeeze dir ratio rect st = zip wins rects
  where wins    = integrate st
        nwins   = length wins
        sizes   = take nwins $ unfoldr (\r -> Just (r * ratio, r * ratio)) 1
        totals' = 0 : zipWith (+) sizes totals'
        totals  = tail totals'
        splits  = zip (tail sizes) totals
        ratios  = reverse $ map (uncurry (/)) splits
        rects   = genRects rect ratios
        genRects r []     = [r]
        genRects r (x:xs) = r' : genRects r'' xs
          where (r', r'') = splitRect r x (dirAxes dir)

splitRect :: Rectangle -> Rational -> Axes -> (Rectangle, Rectangle)
splitRect (Rectangle x y w h) ratio (ax, ay) = (Rectangle x' y' w' h', Rectangle x'' y'' w'' h'')
  where portion = ratio / (ratio + 1)
        w1  = (round $ fi w * portion) :: Int
        w2  = fi w - w1
        h1  = (round $ fi h * portion) :: Int
        h2  = fi h - h1
        x'  = x + fi (negate ax * (1 - ax) * w2 `div` 2)
        y'  = y + fi (negate ay * (1 - ay) * h2 `div` 2)
        w'  = fi $ w1 + (1 - abs ax) * w2
        h'  = fi $ h1 + (1 - abs ay) * h2
        x'' = x + fi (ax * (1 + ax) * w1 `div` 2)
        y'' = y + fi (ay * (1 + ay) * h1 `div` 2)
        w'' = fi $ w2 + (1 - abs ax) * w1
        h'' = fi $ h2 + (1 - abs ay) * h1
        fi :: (Num b, Integral a) => a -> b
        fi  = fromIntegral

type Axes          = (Int, Int)
type AxesGenerator = Axes -> Chirality -> (Axes, Chirality)

dirAxes :: Direction2D -> Axes
dirAxes L = (-1,  0)
dirAxes R = ( 1,  0)
dirAxes U = ( 0, -1)
dirAxes D = ( 0,  1)

alternate :: AxesGenerator
alternate = chDir alt

rotate :: AxesGenerator
rotate = chDir id

chDir :: (Chirality -> Chirality) -> AxesGenerator
chDir f (x, y) r = (a' r, r')
  where a' CW  = (-y,  x)
        a' CCW = ( y, -x)
        r' = f r

alt :: Chirality -> Chirality
alt CW  = CCW
alt CCW = CW
