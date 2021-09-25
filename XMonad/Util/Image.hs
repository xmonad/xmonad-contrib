----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Image
-- Description :  Utilities for manipulating @[[Bool]]@ as images.
-- Copyright   :  (c) 2010 Alejandro Serrano
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  trupill@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities for manipulating [[Bool]] as images
--
-----------------------------------------------------------------------------

module XMonad.Util.Image
    ( -- * Usage:
      -- $usage
      Placement(..),
      iconPosition,
      drawIcon,
    ) where

import XMonad
import XMonad.Util.Font (stringToPixel,fi)

-- | Placement of the icon in the title bar
data Placement = OffsetLeft Int Int   -- ^ An exact amount of pixels from the upper left corner
                 | OffsetRight Int Int  -- ^ An exact amount of pixels from the right left corner
                 | CenterLeft Int        -- ^ Centered in the y-axis, an amount of pixels from the left
                 | CenterRight Int       -- ^ Centered in the y-axis, an amount of pixels from the right
                   deriving (Show, Read)

-- $usage
-- This module uses matrices of boolean values as images. When drawing them,
-- a True value tells that we want the fore color, and a False value that we
-- want the background color to be painted.
-- In the module we suppose that those matrices are represented as [[Bool]],
-- so the lengths of the inner lists must be the same.
--
-- See "XMonad.Layout.Decoration" for usage examples

-- | Gets the ('width', 'height') of an image
imageDims :: [[Bool]] -> (Int, Int)
imageDims img = (length (head img), length img)

-- | Return the 'x' and 'y' positions inside a 'Rectangle' to start drawing
--   the image given its 'Placement'
iconPosition :: Rectangle -> Placement -> [[Bool]] -> (Position,Position)
iconPosition Rectangle{} (OffsetLeft x y) _ = (fi x, fi y)
iconPosition (Rectangle _ _ w _) (OffsetRight x y) icon =
  let (icon_w, _) = imageDims icon
  in (fi w - fi x - fi icon_w, fi y)
iconPosition (Rectangle _ _ _ h) (CenterLeft x) icon =
  let (_, icon_h) = imageDims icon
  in  (fi x, fi (h `div` 2) - fi (icon_h `div` 2))
iconPosition (Rectangle _ _ w h) (CenterRight x) icon =
  let (icon_w, icon_h) = imageDims icon
  in  (fi w - fi x - fi icon_w, fi (h `div` 2) - fi (icon_h `div` 2))

-- | Converts an image represented as [[Bool]] to a series of points
--   to be painted (the ones with True values)
iconToPoints :: [[Bool]] -> [Point]
iconToPoints icon =
  let labels_inside = map (zip (iterate (1+) 0)) icon
      filtered_inside = map (\l -> [x | (x, t) <- l, t]) labels_inside
      labels_outside = zip (iterate (1+) 0) filtered_inside
  in [Point x y | (y, l) <- labels_outside, x <- l]

-- | Displaces a point ('a', 'b') along a vector ('x', 'y')
movePoint :: Position -> Position -> Point -> Point
movePoint x y (Point a b) = Point (a + x) (b + y)

-- | Displaces a list of points along a vector 'x', 'y'
movePoints :: Position -> Position -> [Point] -> [Point]
movePoints x y = map (movePoint x y)

-- | Draw an image into a X surface
drawIcon :: (Functor m, MonadIO m) => Display -> Drawable -> GC -> String
            ->String -> Position -> Position -> [[Bool]] -> m ()
drawIcon dpy drw gc fc bc x y icon = do
  let (i_w, i_h) = imageDims icon
  fcolor <- stringToPixel dpy fc
  bcolor <- stringToPixel dpy bc
  io $ setForeground dpy gc bcolor
  io $ fillRectangle dpy drw gc x y (fi i_w) (fi i_h)
  io $ setForeground dpy gc fcolor
  io $ drawPoints dpy drw gc (movePoints x y (iconToPoints icon)) coordModeOrigin
