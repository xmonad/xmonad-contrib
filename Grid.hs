{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Grid
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

module XMonadContrib.Grid (
    -- * Usage
    -- $usage
	Grid(..)
) where

import XMonad
import StackSet
import Graphics.X11.Xlib.Types

-- $usage
-- Put the following in your Config.hs file:
--
-- > import XMonadContrib.Grid
-- > ...
-- > defaultLayouts = [ ...
-- >                  , Layout Grid
-- >                  ]

-- %import XMonadContrib.Grid
-- %layout , Layout Grid

data Grid a = Grid deriving (Read, Show)

instance LayoutClass Grid a where
    pureLayout Grid r s = arrange r (integrate s)

arrange :: Rectangle -> [a] -> [(a, Rectangle)]
arrange (Rectangle rx ry rw rh) st = zip st rectangles
	where
	nwins = length st
	ncols = ceiling . (sqrt :: Double -> Double) . fromIntegral $ nwins
	mincs = nwins `div` ncols
	extrs = nwins - ncols * mincs
	chop :: Int -> Dimension -> [(Position, Dimension)]
	chop n m = ((0, m - k * fromIntegral (pred n)) :) . map (flip (,) k) . tail . reverse . take n . tail . iterate (subtract k') $ m'
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
