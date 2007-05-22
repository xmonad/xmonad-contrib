module Spiral (spiral) where

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
--                       spiral (1000 % 1618) ]
--
spiral :: Rational -> Layout
spiral rat = Layout { doLayout = \sc ws -> return $ zip ws (divideRects rat (length ws) East $ sc),
                      modifyLayout = \m -> fmap resize (fromMessage m)}

    where resize Expand = let newRat = ((numerator rat + 10) % (denominator rat))
                              normRat = if numerator newRat > denominator newRat then rat else newRat in
                          spiral normRat
          resize Shrink = let newRat = ((numerator rat - 10) % (denominator rat))
                              normRat = if numerator newRat < 0 then rat else newRat in
                          spiral normRat

data Direction = East | South | West | North

nextDir :: Direction -> Direction
nextDir East = South
nextDir South = West
nextDir West = North
nextDir North = East

divideRects :: Rational -> Int -> Direction -> Rectangle -> [Rectangle]
divideRects r n dir rect | n <= 1 = [rect]
                         | otherwise = case divideRect r dir rect of
                                     (r1, r2) -> r1 : (divideRects r (n - 1) (nextDir dir) r2)

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
