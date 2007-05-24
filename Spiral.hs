module XMonadContrib.Spiral (spiral) where

import Graphics.X11.Xlib
import Operations
import Data.Ratio
import XMonad

--
-- Spiral layout
--
-- eg,
--    defaultLayouts :: [Layout]
--    defaultLayouts = [ full,
--                       tall defaultWindowsInMaster defaultDelta (1%2),
--                       wide defaultWindowsInMaster defaultDelta (1%2),
--                       spiral (1 % 1) ]
--
fibs :: [Integer]
fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))

mkRatios :: [Integer] -> [Rational]
mkRatios (x1:x2:xs) = (x1 % x2) : mkRatios (x2:xs)
mkRatios _ = []

data Direction = East | South | West | North deriving (Enum)

spiral :: Rational -> Layout
spiral scale = Layout { doLayout = fibLayout,
                        modifyLayout = \m -> fmap resize (fromMessage m) }
    where
      fibLayout sc ws = return $ zip ws rects
          where len = length ws
                ratios = map (* scale) . reverse . take len . mkRatios $ fibs
                rects = divideRects ratios (cycle [East .. North]) len sc

      resize Expand = spiral $ (21 % 20) * scale
      resize Shrink = spiral $ (20 % 21) * scale

divideRects :: [Rational] -> [Direction] -> Int -> Rectangle -> [Rectangle]
divideRects [] _ _ _ = []
divideRects _ [] _ _ = []
divideRects (r:rs) (d:ds) n rect | n <= 1 = [rect]
                                 | otherwise = case divideRect r d rect of
                                       (r1, r2) -> r1 : (divideRects rs ds (n - 1) r2)

divideRect :: Rational -> Direction -> Rectangle -> (Rectangle, Rectangle)
divideRect ratio East (Rectangle x y w h) = let (w1, w2) = chop ratio (fromIntegral w) in
                                            (Rectangle x y (fromIntegral w1) h,
                                             Rectangle (x + (fromIntegral w1)) y (fromIntegral w2) h)
divideRect ratio South (Rectangle x y w h) = let (h1, h2) = chop ratio (fromIntegral h) in
                                             (Rectangle x y w (fromIntegral h1),
                                              Rectangle x (y + (fromIntegral h1)) w (fromIntegral h2))
divideRect ratio West (Rectangle x y w h) = let (w1, w2) = chop (1 - ratio) (fromIntegral w) in
                                            (Rectangle (x + (fromIntegral w1)) y (fromIntegral w2) h,
                                             Rectangle x y (fromIntegral w1) h)
divideRect ratio North (Rectangle x y w h) = let (h1, h2) = chop (1 - ratio) (fromIntegral h) in
                                             (Rectangle x (y + (fromIntegral h1)) w (fromIntegral h2),
                                              Rectangle x y w (fromIntegral h1))

chop :: Rational -> Integer -> (Integer, Integer)
chop rat n = let f = ((fromIntegral n) * (numerator rat)) `div` (denominator rat) in
             (f, n - f)
