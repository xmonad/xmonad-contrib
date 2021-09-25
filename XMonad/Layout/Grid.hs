{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Grid
-- Description :  A simple layout that attempts to put all windows in a square grid.
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A simple layout that attempts to put all windows in a square grid.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Grid (
    -- * Usage
    -- $usage
    Grid(..), arrange, defaultRatio
) where

import XMonad
import XMonad.StackSet

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Grid
--
-- Then edit your @layoutHook@ by adding the Grid layout:
--
-- > myLayout = Grid ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- You can also specify an aspect ratio for Grid to strive for with the
-- GridRatio constructor.  For example, if you want Grid to try to make a grid
-- four windows wide and three windows tall, you could use
--
-- > myLayout = GridRatio (4/3) ||| etc.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data Grid a = Grid | GridRatio Double deriving (Read, Show)

defaultRatio :: Double
defaultRatio = 16/9

instance LayoutClass Grid a where
    pureLayout Grid          r = pureLayout (GridRatio defaultRatio) r
    pureLayout (GridRatio d) r = arrange d r . integrate

arrange :: Double -> Rectangle -> [a] -> [(a, Rectangle)]
arrange aspectRatio (Rectangle rx ry rw rh) st = zip st rectangles
    where
    nwins = length st
    ncols = max 1 . min nwins . round . sqrt $ fromIntegral nwins * fromIntegral rw / (fromIntegral rh * aspectRatio)
    mincs = max 1 $ nwins `div` ncols
    extrs = nwins - ncols * mincs
    chop :: Int -> Dimension -> [(Position, Dimension)]
    chop n m = ((0, m - k * fromIntegral (pred n)) :) . map (, k) . tail . reverse . take n . tail . iterate (subtract k') $ m'
        where
        k :: Dimension
        k = m `div` fromIntegral n
        m' = fromIntegral m
        k' :: Position
        k' = fromIntegral k
    xcoords = chop ncols rw
    ycoords = chop mincs rh
    ycoords' = chop (succ mincs) rh
    (xbase, xext) = splitAt (ncols - extrs) xcoords
    rectangles = combine ycoords xbase ++ combine ycoords' xext
        where
        combine ys xs = [Rectangle (rx + x) (ry + y) w h | (x, w) <- xs, (y, h) <- ys]
