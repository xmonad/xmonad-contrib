{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

----------------------------------------------------------------------
-- |
-- Module      : XMonad.Layout.GridVariants
-- Description : Two grid layouts.
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
                                    ChangeMasterGridGeom(..)
                                  , ChangeGridGeom(..)
                                  , Grid(..)
                                  , TallGrid(..)
                                  , SplitGrid(..)
                                  , Orientation(..)
                                  ) where

import XMonad.Prelude
import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module can be used as follows:
--
-- > import XMonad.Layout.GridVariants
--
-- Then add something like this to your layouts:
--
-- > Grid (16/10)
--
-- for a 16:10 aspect ratio grid, or
--
-- > SplitGrid L 2 3 (2/3) (16/10) (5/100)
--
-- for a layout with a 2x3 master grid that uses 2/3 of the screen,
-- and a 16:10 aspect ratio slave grid to its right.  The last
-- parameter is again the percentage by which the split between master
-- and slave area changes in response to Expand/Shrink messages.
--
-- To be able to change the geometry of the master grid, add something
-- like this to your keybindings:
--
-- > ((modm .|. shiftMask, xK_equal), sendMessage $ IncMasterCols 1),
-- > ((modm .|. shiftMask, xK_minus), sendMessage $ IncMasterCols (-1)),
-- > ((modm .|. controlMask,  xK_equal), sendMessage $ IncMasterRows 1),
-- > ((modm .|. controlMask,  xK_minus), sendMessage $ IncMasterRows (-1))

-- | Grid layout.  The parameter is the desired x:y aspect ratio of windows
newtype Grid a = Grid Rational
              deriving (Read, Show)

instance LayoutClass Grid a where

    pureLayout (Grid aspect) rect st = zip wins rects
        where
          wins  = W.integrate st
          nwins = length wins
          rects = arrangeAspectGrid rect nwins aspect

    pureMessage layout msg = fmap (changeGridAspect layout) (fromMessage msg)

    description _ = "Grid"

changeGridAspect :: Grid a -> ChangeGridGeom -> Grid a
changeGridAspect (Grid _) (SetGridAspect aspect) = Grid aspect
changeGridAspect (Grid aspect) (ChangeGridAspect delta) =
    Grid (max 0.00001 (aspect + delta))

-- |Geometry change messages understood by Grid and SplitGrid
data ChangeGridGeom
    = SetGridAspect !Rational
    | ChangeGridAspect !Rational

instance Message ChangeGridGeom

-- |SplitGrid layout.  Parameters are
--
--   - side where the master is
--   - number of master rows
--   - number of master columns
--   - portion of screen used for master grid
--   - x:y aspect ratio of slave windows
--   - increment for resize messages
data SplitGrid a = SplitGrid Orientation !Int !Int !Rational !Rational !Rational
                   deriving (Read, Show)

-- |Type to specify the side of the screen that holds
--  the master area of a SplitGrid.
data Orientation = T | B | L | R
                   deriving (Eq, Read, Show)

instance LayoutClass SplitGrid a where

    pureLayout (SplitGrid o mrows mcols mfrac saspect _) rect st = zip wins rects
        where
          wins  = W.integrate st
          nwins = length wins
          rects = arrangeSplitGrid rect o nwins mrows mcols mfrac saspect

    pureMessage layout msg =
        msum [ fmap (resizeMaster layout)          (fromMessage msg)
             , fmap (changeMasterGrid layout)      (fromMessage msg)
             , fmap (changeSlaveGridAspect layout) (fromMessage msg)
             ]

    description _ = "SplitGrid"

-- |The geometry change message understood by the master grid
data ChangeMasterGridGeom
    = IncMasterRows     !Int      -- ^Change the number of master rows
    | IncMasterCols     !Int      -- ^Change the number of master columns
    | SetMasterRows     !Int      -- ^Set the number of master rows to absolute value
    | SetMasterCols     !Int      -- ^Set the number of master columns to absolute value
    | SetMasterFraction !Rational -- ^Set the fraction of the screen used by the master grid

instance Message ChangeMasterGridGeom

arrangeSplitGrid :: Rectangle -> Orientation -> Int -> Int -> Int -> Rational -> Rational -> [Rectangle]
arrangeSplitGrid rect@(Rectangle rx ry rw rh) o nwins mrows mcols mfrac saspect
    | nwins <= mwins = arrangeMasterGrid rect nwins mcols
    | mwins == 0     = arrangeAspectGrid rect nwins saspect
    | otherwise      = arrangeMasterGrid mrect mwins mcols ++
                       arrangeAspectGrid srect swins saspect
    where
      mwins            = mrows * mcols
      swins            = nwins - mwins
      mrect            = Rectangle mx my mw mh
      srect            = Rectangle sx sy sw sh
      (mh, sh, mw, sw) = if o `elem` [T, B] then
                             (ceiling (fromIntegral rh * mfrac), rh - mh, rw, rw)
                         else
                             (rh, rh, ceiling (fromIntegral rw * mfrac), rw - mw)
      mx               = fromIntegral rx + if o == R then fromIntegral sw else 0
      my               = fromIntegral ry + if o == B then fromIntegral sh else 0
      sx               = fromIntegral rx + if o == L then fromIntegral mw else 0
      sy               = fromIntegral ry + if o == T then fromIntegral mh else 0

arrangeMasterGrid :: Rectangle -> Int -> Int -> [Rectangle]
arrangeMasterGrid rect nwins mcols = arrangeGrid rect nwins (min nwins mcols)

arrangeAspectGrid :: Rectangle -> Int -> Rational -> [Rectangle]
arrangeAspectGrid rect@(Rectangle _ _ rw rh) nwins aspect =
    arrangeGrid rect nwins (min nwins ncols)
    where
      scr_a = fromIntegral rw / fromIntegral rh
      fcols = sqrt ( fromRational $ scr_a * fromIntegral nwins / aspect ) :: Double
      cols1 = floor fcols :: Int
      cols2 = ceiling fcols :: Int
      rows1 = ceiling ( fromIntegral nwins / fromIntegral cols1 :: Rational ) :: Int
      rows2 = floor ( fromIntegral nwins / fromIntegral cols2 :: Rational ) :: Int
      a1    = scr_a * fromIntegral rows1 / fromIntegral cols1
      a2    = scr_a * fromIntegral rows2 / fromIntegral cols2
      ncols | cols1 == 0                = cols2
            | rows2 == 0                = cols1
            | a1 / aspect < aspect / a2 = cols1
            | otherwise                 = cols2

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
      rects         = concat rects_in_cols

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
      offsets = take (fromIntegral extra) [1..] ++ [extra,extra..]

resizeMaster :: SplitGrid a -> Resize -> SplitGrid a
resizeMaster (SplitGrid o mrows mcols mfrac saspect delta) Shrink =
    SplitGrid o mrows mcols (max 0 (mfrac - delta)) saspect delta
resizeMaster (SplitGrid o mrows mcols mfrac saspect delta) Expand =
    SplitGrid o mrows mcols (min 1 (mfrac + delta)) saspect delta

changeMasterGrid :: SplitGrid a -> ChangeMasterGridGeom -> SplitGrid a
changeMasterGrid (SplitGrid o mrows mcols mfrac saspect delta) (IncMasterRows d) =
    SplitGrid o (max 0 (mrows + d)) mcols mfrac saspect delta
changeMasterGrid (SplitGrid o mrows mcols mfrac saspect delta) (IncMasterCols d) =
    SplitGrid o mrows (max 0 (mcols + d)) mfrac saspect delta
changeMasterGrid (SplitGrid o _ mcols mfrac saspect delta) (SetMasterRows mrows) =
    SplitGrid o (max 0 mrows) mcols mfrac saspect delta
changeMasterGrid (SplitGrid o mrows _ mfrac saspect delta) (SetMasterCols mcols) =
    SplitGrid o mrows (max 0 mcols) mfrac saspect delta
changeMasterGrid (SplitGrid o mrows mcols _ saspect delta) (SetMasterFraction mfrac) =
    SplitGrid o mrows mcols mfrac saspect delta

changeSlaveGridAspect :: SplitGrid a -> ChangeGridGeom -> SplitGrid a
changeSlaveGridAspect (SplitGrid o mrows mcols mfrac _ delta) (SetGridAspect saspect) =
    SplitGrid o mrows mcols mfrac saspect delta
changeSlaveGridAspect (SplitGrid o mrows mcols mfrac saspect delta) (ChangeGridAspect sdelta) =
    SplitGrid o mrows mcols mfrac (max 0.00001 (saspect + sdelta)) delta

-- | TallGrid layout.  Parameters are
--
--   - number of master rows
--   - number of master columns
--   - portion of screen used for master grid
--   - x:y aspect ratio of slave windows
--   - increment for resize messages
--
--   This exists mostly because it was introduced in an earlier version.
--   It's a fairly thin wrapper around "SplitGrid L".
data TallGrid a = TallGrid !Int !Int !Rational !Rational !Rational
                  deriving (Read, Show)

instance LayoutClass TallGrid a where

    pureLayout (TallGrid mrows mcols mfrac saspect _) rect st = zip wins rects
        where
          wins  = W.integrate st
          nwins = length wins
          rects = arrangeSplitGrid rect L nwins mrows mcols mfrac saspect

    pureMessage layout msg =
        msum [ fmap (tallGridAdapter resizeMaster layout) (fromMessage msg)
             , fmap (tallGridAdapter changeMasterGrid layout) (fromMessage msg) ]

    description _ = "TallGrid"

tallGridAdapter :: (SplitGrid a -> b -> SplitGrid a) -> TallGrid a -> b -> TallGrid a
tallGridAdapter f (TallGrid mrows mcols mfrac saspect delta) msg =
    TallGrid mrows' mcols' mfrac' saspect' delta'
    where
      SplitGrid _ mrows' mcols' mfrac' saspect' delta' =
          f (SplitGrid L mrows mcols mfrac saspect delta) msg
