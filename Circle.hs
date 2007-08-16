-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Circle
-- Copyright   :  (c) Peter De Wachter
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Circle is an elliptical, overlapping layout, by Peter De Wachter
--
-----------------------------------------------------------------------------

module XMonadContrib.Circle (
                             -- * Usage
                             -- $usage
                             circle
                            ) where -- actually it's an ellipse

import Data.List
import Graphics.X11.Xlib
import XMonad
import StackSet (integrate, peek)

import XMonadContrib.LayoutHelpers ( idModify )

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.Circle

circle :: Layout Window
circle = Layout { doLayout = \r s -> do { layout <- raiseFocus $ circleLayout r $ integrate s
                                       ;  return (layout, Nothing) }
                , modifyLayout = idModify }

circleLayout :: Rectangle -> [a] -> [(a, Rectangle)]
circleLayout _ []     = []
circleLayout r (w:ws) = master : rest
    where master = (w, center r)
          rest   = zip ws $ map (satellite r) [0, pi * 2 / fromIntegral (length ws) ..]

raiseFocus :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
raiseFocus xs = do focused <- withWindowSet (return . peek)
                   return $ case find ((== focused) . Just . fst) xs of
                              Just x  -> x : delete x xs
                              Nothing -> xs

center :: Rectangle -> Rectangle
center (Rectangle sx sy sw sh) = Rectangle x y w h
    where s = sqrt 2 :: Double
          w = round (fromIntegral sw / s)
          h = round (fromIntegral sh / s)
          x = sx + fromIntegral (sw - w) `div` 2
          y = sy + fromIntegral (sh - h) `div` 2

satellite :: Rectangle -> Double -> Rectangle
satellite (Rectangle sx sy sw sh) a = Rectangle (sx + round (rx + rx * cos a))
                                                (sy + round (ry + ry * sin a))
                                                w h
    where rx = fromIntegral (sw - w) / 2
          ry = fromIntegral (sh - h) / 2
          w = sw * 10 `div` 25
          h = sh * 10 `div` 25

