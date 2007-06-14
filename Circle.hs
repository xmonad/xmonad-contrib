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

import Graphics.X11.Xlib
import XMonad
import StackSet (integrate)

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.Circle

circle :: Layout
circle = Layout { doLayout = \r -> circleLayout r . integrate,
                  modifyLayout = return . const Nothing }

circleLayout :: Rectangle -> [Window] -> X [(Window, Rectangle)]
circleLayout _ []     = return []
circleLayout r (w:ws) = return $ (w, center r) : (zip ws sats)
    where sats = map (satellite r) $ take (length ws) [0, pi * 2 / fromIntegral (length ws) ..]

center :: Rectangle -> Rectangle
center (Rectangle sx sy sw sh) = Rectangle x y w h
    where w = round ((fromIntegral sw / sqrt 2) :: Double)
          h = round ((fromIntegral sh / sqrt 2) :: Double)
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

