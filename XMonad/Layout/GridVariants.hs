{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------
-- |
-- Module      : XMonad.Layout.GridVariants
-- Copyright   : (c) Norbert Zeh
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : nzeh@cs.dal.ca
-- Stability   : unstable
-- Portability : unportable
--
-- Two layouts: one is a variant of the Grid layout that allows the
-- desired aspect ratio of windows to be specified.  The other is like
-- Tall but places a grid with fixed number of rows and columns in the
-- master area and uses an aspect-ratio-specified layout for the
-- slaves.
----------------------------------------------------------------------

module XMonad.Layout.GridVariants ( -- * Usage
                                    -- $usage
                                    ChangeMasterGeom(..)
                                  , Grid(..)
                                  , TallGrid(..)
                                  ) where

import Control.Monad
import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module can be used as follows:
--
-- > import XMonad.Layout.Master
--
-- Then add something like this to your layouts:
--
-- > Grid (16/10)
--
-- for a 16:10 aspect ratio grid, or
--
-- > TallGrid 2 3 (2/3) (16/10) (5/100)
--
-- for a layout with a 2x3 master grid that uses 2/3 of the screen,
-- and a 16:10 aspect ratio slave grid.  The last parameter is again
-- the percentage by which the split between master and slave area
-- changes in response to Expand/Shrink messages.
--
-- To be able to change the geometry of the master grid, add something
-- like this to your keybindings:
--
-- > ((modMask .|. shiftMask, xK_equal), sendMessage $ IncMasterCols 1),
-- > ((modMask .|. shiftMask, xK_minus), sendMessage $ IncMasterCols (-1)),
-- > ((modMask .|. ctrlMask,  xK_equal), sendMessage $ IncMasterRows 1),
-- > ((modMask .|. ctrlMask,  xK_minus), sendMessage $ IncMasterRows (-1))

-- | Grid layout.  The parameter is the desired x:y aspect ratio of windows
data Grid a = Grid !Rational
              deriving (Read, Show)

instance LayoutClass Grid a where

    pureLayout (Grid aspect) rect st = zip wins rects
        where
          wins  = W.integrate st
          nwins = length wins
          rects = arrangeAspectGrid rect nwins aspect

    description _ = "Grid"

-- | TallGrid layout.  Parameters are
--
--   - number of master rows
--   - number of master columns
--   - portion of screen used for master grid
--   - x:y aspect ratio of slave windows
--   - increment for resize messages
data TallGrid a = TallGrid !Int !Int !Rational !Rational !Rational
                  deriving (Read, Show)

instance LayoutClass TallGrid a where

    pureLayout (TallGrid mrows mcols mfrac saspect _) rect st = zip wins rects
        where
          wins  = W.integrate st
          nwins = length wins
          rects = arrangeTallGrid rect nwins mrows mcols mfrac saspect

    pureMessage layout msg =
        msum [ fmap (resizeMaster layout) (fromMessage msg)
             , fmap (changeMasterGrid layout) (fromMessage msg) ]

    description _ = "TallGrid"

-- |The geometry change message understood by the master grid
data ChangeMasterGeom
    = IncMasterRows !Int -- ^Change the number of master rows
    | IncMasterCols !Int -- ^Change the number of master columns
    deriving Typeable

instance Message ChangeMasterGeom

arrangeTallGrid :: Rectangle -> Int -> Int -> Int -> Rational -> Rational -> [Rectangle]
arrangeTallGrid rect@(Rectangle rx ry rw rh) nwins mrows mcols mfrac saspect
    | nwins <= mwins = arrangeMasterGrid rect nwins mcols
    | mwins == 0     = arrangeAspectGrid rect nwins saspect
    | otherwise      = (arrangeMasterGrid mrect mwins mcols) ++
                       (arrangeAspectGrid srect swins saspect)
    where
      mwins = mrows * mcols
      swins = nwins - mwins
      mrect = Rectangle rx ry rw mh
      srect = Rectangle rx (fromIntegral ry + fromIntegral mh) rw sh
      mh    = ceiling (fromIntegral rh * mfrac)
      sh    = rh - mh

arrangeMasterGrid :: Rectangle -> Int -> Int -> [Rectangle]
arrangeMasterGrid rect nwins mcols = arrangeGrid rect nwins (min nwins mcols)

arrangeAspectGrid :: Rectangle -> Int -> Rational -> [Rectangle]
arrangeAspectGrid rect@(Rectangle _ _ rw rh) nwins aspect =
    arrangeGrid rect nwins (min nwins ncols)
    where
      ncols = ceiling $ sqrt $ ( fromRational
              ( (fromIntegral rw * fromIntegral nwins) / (fromIntegral rh * aspect) ) :: Double)

arrangeGrid :: Rectangle -> Int -> Int -> [Rectangle]
arrangeGrid (Rectangle rx ry rw rh) nwins ncols =
    [Rectangle (fromIntegral x + rx) (fromIntegral y + ry) (fromIntegral w) (fromIntegral h)
     | (x, y, w, h) <- rects]
    where
      nrows_in_cols = listDifference $ splitEvenly nwins ncols
      x_slabs       = splitIntoSlabs (fromIntegral rw) ncols
      y_slabs       = [splitIntoSlabs (fromIntegral rh) nrows | nrows <- nrows_in_cols]
      rects_in_cols = [[(x, y, w, h) | (y, h) <- lst]
                       | ((x, w), lst) <- zip x_slabs y_slabs]
      rects         = foldr (++) [] rects_in_cols

splitIntoSlabs :: Int -> Int -> [(Int, Int)]
splitIntoSlabs width nslabs = zip (0:xs) widths
    where
      xs = splitEvenly width nslabs
      widths = listDifference xs

listDifference :: [Int] -> [Int]
listDifference lst = [cur-pre | (cur,pre) <- zip lst (0:lst)]

splitEvenly :: Int -> Int -> [Int]
splitEvenly n parts = [ sz-off | (sz,off) <- zip sizes offsets]
    where
      size    = ceiling ( (fromIntegral n / fromIntegral parts) :: Double )
      extra   = size*parts - n
      sizes   = [i*size | i <- [1..parts]]
      offsets = (take (fromIntegral extra) [1..]) ++ [extra,extra..]

resizeMaster :: TallGrid a -> Resize -> TallGrid a
resizeMaster (TallGrid mrows mcols mfrac saspect delta) Shrink =
    TallGrid mrows mcols (max 0 (mfrac - delta)) saspect delta
resizeMaster (TallGrid mrows mcols mfrac saspect delta) Expand =
    TallGrid mrows mcols (min 1 (mfrac + delta)) saspect delta

changeMasterGrid :: TallGrid a -> ChangeMasterGeom -> TallGrid a
changeMasterGrid (TallGrid mrows mcols mfrac saspect delta) (IncMasterRows d) =
    TallGrid (max 0 (mrows + d)) mcols mfrac saspect delta
changeMasterGrid (TallGrid mrows mcols mfrac saspect delta) (IncMasterCols d) =
    TallGrid mrows (max 0 (mcols + d)) mfrac saspect delta
