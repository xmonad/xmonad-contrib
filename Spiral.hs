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
                        modifyLayout = \m -> fmap resize $ fromMessage m }
    where
      fibLayout sc ws = return $ zip ws rects
          where ratios = map (* scale) . reverse . take (length ws) . mkRatios $ fibs
                rects = divideRects (zip ratios (cycle [East .. North])) sc

      resize Expand = spiral $ (21 % 20) * scale
      resize Shrink = spiral $ (20 % 21) * scale

divideRects :: [(Rational, Direction)] -> Rectangle -> [Rectangle]
divideRects [] _ = []
divideRects [_] r = [r]
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
