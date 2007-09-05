-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Spiral
-- Copyright   :  (c) Joe Thornber <joe.thornber@gmail.com>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Joe Thornber <joe.thornber@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Spiral adds a spiral tiling layout
--
-----------------------------------------------------------------------------

module XMonadContrib.Spiral (
                             -- * Usage
                             -- $usage
                             spiral
                            , spiralWithDir
                            , Rotation (..)
                            , Direction (..)
                            ) where

import Graphics.X11.Xlib
import Operations
import Data.Ratio
import XMonad

import XMonadContrib.LayoutHelpers

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- >   import XMonadContrib.Spiral
--
-- >   defaultLayouts = [ full, spiral (1 % 1), ... ]

-- %import XMonadContrib.Spiral
-- %layout , spiral (1 % 1)

fibs :: [Integer]
fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))

mkRatios :: [Integer] -> [Rational]
mkRatios (x1:x2:xs) = (x1 % x2) : mkRatios (x2:xs)
mkRatios _ = []

data Rotation = CW | CCW
data Direction = East | South | West | North deriving (Eq, Enum)

blend :: Rational -> [Rational] -> [Rational]
blend scale ratios = zipWith (+) ratios scaleFactors
    where
      len = length ratios
      step = (scale - (1 % 1)) / (fromIntegral len)
      scaleFactors = map (* step) . reverse . take len $ [0..]

spiral :: Rational -> Layout a
spiral = spiralWithDir East CW

spiralWithDir :: Direction -> Rotation -> Rational -> Layout a
spiralWithDir dir rot scale = Layout { doLayout = l2lModDo fibLayout,
                                       modifyLayout = \m -> return $ fmap resize $ fromMessage m }
    where
      fibLayout sc ws = zip ws rects
          where ratios = blend scale . reverse . take (length ws - 1) . mkRatios $ tail fibs
                rects = divideRects (zip ratios dirs) sc
                dirs  = dropWhile (/= dir) $ case rot of
                                               CW  -> cycle [East .. North] 
                                               CCW -> cycle [North, West, South, East]
      resize Expand = spiralWithDir dir rot $ (21 % 20) * scale
      resize Shrink = spiralWithDir dir rot $ (20 % 21) * scale

-- This will produce one more rectangle than there are splits details
divideRects :: [(Rational, Direction)] -> Rectangle -> [Rectangle]
divideRects [] r = [r]
divideRects ((r,d):xs) rect = case divideRect r d rect of
                                (r1, r2) -> r1 : (divideRects xs r2)

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
chop rat n = let f = ((fromIntegral n) * (numerator rat)) `div` (denominator rat) in
             (f, n - f)
