-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Rectangle
-- Description :  A module for handling pixel rectangles.
-- Copyright   :  (c) 2018 Yclept Nemo
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for handling pixel rectangles: 'Rectangle'.
--
-----------------------------------------------------------------------------


module XMonad.Util.Rectangle
    ( -- * Usage
      -- $usage
      PointRectangle (..)
    , pixelsToIndices, pixelsToCoordinates
    , indicesToRectangle, coordinatesToRectangle
    , empty
    , intersects
    , supersetOf
    , difference
    , withBorder
    , center
    , toRatio
    ) where

import           XMonad
import           XMonad.Prelude (fi)
import qualified XMonad.StackSet as W

import           Data.Ratio


-- $usage
-- > import XMonad.Util.Rectangle as R
-- > R.empty (Rectangle 0 0 1024 768)


-- | Rectangle as two points. What those points mean depends on the conversion
-- function.
data PointRectangle a = PointRectangle
    { point_x1::a   -- ^ Point nearest to the origin.
    , point_y1::a
    , point_x2::a   -- ^ Point furthest from the origin.
    , point_y2::a
    } deriving (Eq,Read,Show)

-- | There are three possible ways to convert rectangles to pixels:
--
-- * Consider integers as "gaps" between pixels; pixels range from @(N,N+1)@,
-- exclusively: @(0,1)@, @(1,2)@, and so on. This leads to interval ambiguity:
-- whether an integer endpoint contains a pixel depends on which direction the
-- interval approaches the pixel. Consider the adjacent pixels @(0,1)@ and
-- @(1,2)@ where @1@ can refer to either pixel @(0,1)@ or pixel @(1,2)@.
--
-- * Consider integers to demarcate the start of each pixel; pixels range from
-- @[N,N+1)@: @[0,1)@, @[1,2)@, and so on - or equivalently: @(N,N+1]@. This is
-- the most flexible coordinate system, and the convention used by the
-- 'Rectangle' type.
--
-- * Consider integers to demarcate the center of each pixel; pixels range from
-- @[N,N+1]@, as though each real-valued coordinate had been rounded (either
-- down or up) to the nearest integers. So each pixel, from zero, is listed as:
-- @[0,0]@, @[1,1]@, @[2,2]@, and so on. Rather than a coordinate system, this
-- considers pixels as row/colum indices.  While easiest to reason with,
-- indices are unable to represent zero-dimension rectangles.
--
-- Consider pixels as indices. Do not use this on empty rectangles.
pixelsToIndices :: Rectangle -> PointRectangle Integer
pixelsToIndices (Rectangle px py dx dy) =
    PointRectangle (fromIntegral px)
                   (fromIntegral py)
                   (fromIntegral px + fromIntegral dx - 1)
                   (fromIntegral py + fromIntegral dy - 1)

-- | Consider pixels as @[N,N+1)@ coordinates. Available for empty rectangles.
pixelsToCoordinates :: Rectangle -> PointRectangle Integer
pixelsToCoordinates (Rectangle px py dx dy) =
    PointRectangle (fromIntegral px)
                   (fromIntegral py)
                   (fromIntegral px + fromIntegral dx)
                   (fromIntegral py + fromIntegral dy)

-- | Invert 'pixelsToIndices'.
indicesToRectangle :: PointRectangle Integer -> Rectangle
indicesToRectangle (PointRectangle x1 y1 x2 y2) =
    Rectangle (fromIntegral x1)
              (fromIntegral y1)
              (fromIntegral $ x2 - x1 + 1)
              (fromIntegral $ y2 - y1 + 1)

-- | Invert 'pixelsToCoordinates'.
coordinatesToRectangle :: PointRectangle Integer -> Rectangle
coordinatesToRectangle (PointRectangle x1 y1 x2 y2) =
    Rectangle (fromIntegral x1)
              (fromIntegral y1)
              (fromIntegral $ x2 - x1)
              (fromIntegral $ y2 - y1)

-- | True if either the 'rect_width' or 'rect_height' fields are zero, i.e. the
-- rectangle has no area.
empty :: Rectangle -> Bool
empty (Rectangle _ _ _ 0) = True
empty (Rectangle _ _ 0 _) = True
empty Rectangle{}         = False

-- | True if the intersection of the set of points comprising each rectangle is
-- not the empty set. Therefore any rectangle containing the initial points of
-- an empty rectangle will never intersect that rectangle - including the same
-- empty rectangle.
intersects :: Rectangle -> Rectangle -> Bool
intersects r1 r2 | empty r1 || empty r2 = False
                 | otherwise            =    r1_x1 < r2_x2
                                          && r1_x2 > r2_x1
                                          && r1_y1 < r2_y2
                                          && r1_y2 > r2_y1
    where PointRectangle r1_x1 r1_y1 r1_x2 r1_y2 = pixelsToCoordinates r1
          PointRectangle r2_x1 r2_y1 r2_x2 r2_y2 = pixelsToCoordinates r2

-- | True if the first rectangle contains at least all the points of the second
-- rectangle. Any rectangle containing the initial points of an empty rectangle
-- will be a superset of that rectangle - including the same empty rectangle.
supersetOf :: Rectangle -> Rectangle -> Bool
supersetOf r1 r2 =    r1_x1 <= r2_x1
                   && r1_y1 <= r2_y1
                   && r1_x2 >= r2_x2
                   && r1_y2 >= r2_y2
    where PointRectangle r1_x1 r1_y1 r1_x2 r1_y2 = pixelsToCoordinates r1
          PointRectangle r2_x1 r2_y1 r2_x2 r2_y2 = pixelsToCoordinates r2

-- | Return the smallest set of rectangles resulting from removing all the
-- points of the second rectangle from those of the first, i.e. @r1 - r2@, such
-- that @0 <= l <= 4@ where @l@ is the length of the resulting list.
difference :: Rectangle -> Rectangle -> [Rectangle]
difference r1 r2 | r1 `intersects` r2 = map coordinatesToRectangle $
                                        concat [rt,rr,rb,rl]
                 | otherwise          = [r1]
    where PointRectangle r1_x1 r1_y1 r1_x2 r1_y2 = pixelsToCoordinates r1
          PointRectangle r2_x1 r2_y1 r2_x2 r2_y2 = pixelsToCoordinates r2
          -- top - assuming (0,0) is top-left
          rt = [PointRectangle (max r2_x1 r1_x1) r1_y1 r1_x2 r2_y1 | r2_y1 > r1_y1 && r2_y1 < r1_y2]
          -- right
          rr = [PointRectangle r2_x2 (max r2_y1 r1_y1) r1_x2 r1_y2 | r2_x2 > r1_x1 && r2_x2 < r1_x2]
          -- bottom
          rb = [PointRectangle r1_x1 r2_y2 (min r2_x2 r1_x2) r1_y2 | r2_y2 > r1_y1 && r2_y2 < r1_y2]
          -- left
          rl = [PointRectangle r1_x1 r1_y1 r2_x1 (min r2_y2 r1_y2) | r2_x1 > r1_x1 && r2_x1 < r1_x2]

-- | Fit a 'Rectangle' within the given borders of itself. Given insufficient
-- space, borders are minimized while preserving the ratio of opposite borders.
-- Origin is top-left, and yes, negative borders are allowed.
withBorder :: Integer -- ^ Top border.
           -> Integer -- ^ Bottom border.
           -> Integer -- ^ Right border.
           -> Integer -- ^ Left border.
           -> Integer -- ^ Smallest allowable rectangle dimensions, i.e.
                      --   width/height, with values @<0@ defaulting to @0@.
           -> Rectangle -> Rectangle
withBorder t b r l i (Rectangle x y w h) =
    let -- conversions
        w' = fromIntegral w
        h' = fromIntegral h
        -- minimum window dimensions
        i' = max i 0
        iw = min i' w'
        ih = min i' h'
        -- maximum border dimensions
        bh = w' - iw
        bv = h' - ih
        -- scaled border ratios
        rh = if l + r <= 0
             then 1
             else min 1 $ bh % (l + r)
        rv = if t + b <= 0
             then 1
             else min 1 $ bv % (t + b)
        -- scaled border pixels
        t' = truncate $ rv * fromIntegral t
        b' = truncate $ rv * fromIntegral b
        r' = truncate $ rh * fromIntegral r
        l' = truncate $ rh * fromIntegral l
    in  Rectangle (x + l')
                  (y + t')
                  (w - r' - fromIntegral l')
                  (h - b' - fromIntegral t')

-- | Calculate the center - @(x,y)@ - as if the 'Rectangle' were bounded.
center :: Rectangle -> (Ratio Integer,Ratio Integer)
center (Rectangle x y w h) = (cx,cy)
    where cx = fromIntegral x + fromIntegral w % 2
          cy = fromIntegral y + fromIntegral h % 2

-- | Invert 'scaleRationalRect'. Since that operation is lossy a roundtrip
-- conversion may not result in the original value. The first 'Rectangle' is
-- scaled to the second:
--
-- >>> (Rectangle 2 2 6 6) `toRatio` (Rectangle 0 0 10 10)
-- RationalRect (1 % 5) (1 % 5) (3 % 5) (3 % 5)
toRatio :: Rectangle -> Rectangle -> W.RationalRect
toRatio (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
    W.RationalRect ((fi x1 - fi x2) / fi w2)
                   ((fi y1 - fi y2) / fi h2)
                   (fi w1 / fi w2) (fi h1 / fi h2)
