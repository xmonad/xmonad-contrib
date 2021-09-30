{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Spiral
-- Description :  A spiral tiling layout.
-- Copyright   :  (c) Joe Thornber <joe.thornber@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Joe Thornber <joe.thornber@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A spiral tiling layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Spiral (
                             -- * Usage
                             -- $usage
                             spiral
                            , spiralWithDir
                            , Rotation (..)
                            , Direction (..)

                            , SpiralWithDir
                            ) where

import Data.Ratio
import XMonad hiding ( Rotation )
import XMonad.StackSet ( integrate )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Spiral
--
-- Then edit your @layoutHook@ by adding the Spiral layout:
--
-- > myLayout =  spiral (6/7) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

mkRatios :: [Integer] -> [Rational]
mkRatios (x1:x2:xs) = (x1 % x2) : mkRatios (x2:xs)
mkRatios _ = []

data Rotation = CW | CCW deriving (Read, Show)
data Direction = East | South | West | North deriving (Eq, Enum, Read, Show)

blend :: Rational -> [Rational] -> [Rational]
blend scale ratios = zipWith (+) ratios scaleFactors
    where
      len = length ratios
      step = (scale - (1 % 1)) / fromIntegral len
      scaleFactors = map (* step) . reverse . take len $ [0..]

-- | A spiral layout.  The parameter controls the size ratio between
--   successive windows in the spiral.  Sensible values range from 0
--   up to the aspect ratio of your monitor (often 4\/3).
--
--   By default, the spiral is counterclockwise, starting to the east.
--   See also 'spiralWithDir'.
spiral :: Rational -> SpiralWithDir a
spiral = spiralWithDir East CW

-- | Create a spiral layout, specifying the starting cardinal direction,
--   the spiral direction (clockwise or counterclockwise), and the
--   size ratio.
spiralWithDir :: Direction -> Rotation -> Rational -> SpiralWithDir a
spiralWithDir = SpiralWithDir

data SpiralWithDir a = SpiralWithDir Direction Rotation Rational
                     deriving ( Read, Show )

instance LayoutClass SpiralWithDir a where
    pureLayout (SpiralWithDir dir rot scale) sc stack = zip ws rects
        where ws = integrate stack
              ratios = blend scale . reverse . take (length ws - 1) . mkRatios $ tail fibs
              rects = divideRects (zip ratios dirs) sc
              dirs  = dropWhile (/= dir) $ case rot of
                                           CW  -> cycle [East .. North]
                                           CCW -> cycle [North, West, South, East]
    handleMessage (SpiralWithDir dir rot scale) = return . fmap resize . fromMessage
        where resize Expand = spiralWithDir dir rot $ (21 % 20) * scale
              resize Shrink = spiralWithDir dir rot $ (20 % 21) * scale
    description _ = "Spiral"

-- This will produce one more rectangle than there are splits details
divideRects :: [(Rational, Direction)] -> Rectangle -> [Rectangle]
divideRects [] r = [r]
divideRects ((r,d):xs) rect = case divideRect r d rect of
                                (r1, r2) -> r1 : divideRects xs r2

-- It's much simpler if we work with all Integers and convert to
-- Rectangle at the end.
data Rect = Rect Integer Integer Integer Integer

fromRect :: Rect -> Rectangle
fromRect (Rect x y w h) = Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

toRect :: Rectangle -> Rect
toRect (Rectangle x y w h) = Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

divideRect :: Rational -> Direction -> Rectangle -> (Rectangle, Rectangle)
divideRect r d rect = let (r1, r2) = divideRect' r d $ toRect rect in
                      (fromRect r1, fromRect r2)

divideRect' :: Rational -> Direction -> Rect -> (Rect, Rect)
divideRect' ratio dir (Rect x y w h) =
    case dir of
      East -> let (w1, w2) = chop ratio w in (Rect x y w1 h, Rect (x + w1) y w2 h)
      South -> let (h1, h2) = chop ratio h in (Rect x y w h1, Rect x (y + h1) w h2)
      West -> let (w1, w2) = chop (1 - ratio) w in (Rect (x + w1) y w2 h, Rect x y w1 h)
      North -> let (h1, h2) = chop (1 - ratio) h in (Rect x (y + h1) w h2, Rect x y w h1)

chop :: Rational -> Integer -> (Integer, Integer)
chop rat n = let f = (fromIntegral n * numerator rat) `div` denominator rat in
             (f, n - f)
