{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.MosaicAlt
-- Copyright   :  (c) 2007 James Webb
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  xmonad#jwebb,sygneca,com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout which gives each window a specified amount of screen space 
-- relative to the others. Compared to the 'Mosaic' layout, this one
-- divides the space in a more balanced way.
--
-----------------------------------------------------------------------------

module XMonadContrib.MosaicAlt (
        -- * Usage:
        -- $usage
        MosaicAlt(..)
        , shrinkWindowAlt
        , expandWindowAlt
        , resetAlt
    ) where

import XMonad
import Operations
import Graphics.X11.Xlib
import qualified StackSet as W
import qualified Data.Map as M
import Data.List ( sortBy )
import Data.Ratio
import Graphics.X11.Types ( Window )

-- $usage
-- You can use this module with the following in your configuration file:
--
-- > import XMonadContrib.MosaicAlt
--
-- > defaultLayouts = ...
-- >                  , SomeLayout $ MosaicAlt M.empty
-- >                  ...
--
-- > keys = ...
-- >     , ((modMask .|. shiftMask, xK_a), withFocused (sendMessage . expandWindowAlt))
-- >     , ((modMask .|. shiftMask, xK_z), withFocused (sendMessage . shrinkWindowAlt))
-- >     , ((modMask .|. controlMask, xK_space), sendMessage resetAlt)
-- >     ...

-- %import XMonadContrib.MosaicAlt
-- %layout , SomeLayout $ MosaicAlt M.empty

data HandleWindowAlt =
    ShrinkWindowAlt Window
    | ExpandWindowAlt Window
    | ResetAlt
    deriving ( Typeable, Eq )
instance Message HandleWindowAlt
shrinkWindowAlt, expandWindowAlt :: Window -> HandleWindowAlt
shrinkWindowAlt = ShrinkWindowAlt
expandWindowAlt = ExpandWindowAlt
resetAlt :: HandleWindowAlt
resetAlt = ResetAlt

type Areas = M.Map Window Rational
data MosaicAlt a = MosaicAlt Areas deriving ( Show, Read )

instance LayoutClass MosaicAlt Window where
    description _ = "MosaicAlt"
    doLayout (MosaicAlt areas) rect stack =
            return (arrange rect stack areas', Just $ MosaicAlt areas')
        where
            areas' = ins (W.up stack) $ ins (W.down stack) $ ins [W.focus stack] areas
            ins wins as = foldl M.union as $ map (`M.singleton` 1) wins

    handleMessage (MosaicAlt areas) msg = return $ case fromMessage msg of
        Just (ShrinkWindowAlt w) -> Just $ MosaicAlt $ alter areas w (4 % 5)
        Just (ExpandWindowAlt w) -> Just $ MosaicAlt $ alter areas w (6 % 5)
        Just ResetAlt -> Just $ MosaicAlt M.empty
        _ -> Nothing

-- Layout algorithm entry point.
arrange :: Rectangle -> W.Stack Window -> Areas -> [(Window, Rectangle)]
arrange rect stack areas = tree rect (sortBy areaCompare winList) totalArea areas
    where
        winList = reverse (W.up stack) ++ W.focus stack : W.down stack
        totalArea = areaSum areas winList
        areaCompare a b = or1 b `compare` or1 a
        or1 w = maybe 1 id $ M.lookup w areas

-- Selects a horizontal or vertical split to get the best aspect ratio.
-- FIXME: Give the user more dynamic control.
splitBest :: Rational -> Rectangle -> (Rectangle, Rectangle)
splitBest ratio rect =
        if (w % h) < cutoff then splitVerticallyBy ratio rect
            else splitHorizontallyBy ratio rect
    where
        -- Prefer wide windows to tall ones, mainly because it makes xterms more usable.
        cutoff = if w > 1000 then 1.25
            else if w < 500 then 2.25
            else 2.25 - (w - 500) % 500
        w = rect_width rect
        h = rect_height rect

-- Recursively group windows into a binary tree. Aim to balance the tree
-- according to the total requested area in each branch.
tree :: Rectangle -> [Window] -> Rational -> Areas -> [(Window, Rectangle)]
tree rect winList totalArea areas = case winList of
    [] -> []
    [x] -> [(x, rect)]
    _ -> tree aRect aWins aArea areas ++ tree bRect bWins bArea areas
        where
            (aRect, bRect) = splitBest (aArea / (aArea + bArea)) rect
            ((aWins, aArea), (bWins, bArea)) = areaSplit areas winList totalArea

-- Sum the requested areas of a bunch of windows.
areaSum :: Areas -> [Window] -> Rational
areaSum areas = sum . map (maybe 1 id . flip M.lookup areas)

-- Split a list of windows in half by area.
areaSplit :: Areas -> [Window] -> Rational -> (([Window], Rational), ([Window], Rational))
areaSplit areas wins totalArea = ((reverse aWins, aArea), (bWins, bArea))
    where
        ((aWins, aArea), (bWins, bArea)) = gather [] wins 0
        gather a b t = if t >= (totalArea / 2) then ((a, t), (b, totalArea - t))
            else gather (head b : a) (tail b) (t + or1 (head b))
        or1 w = maybe 1 id $ M.lookup w areas

-- Change requested area for a window.
alter :: Areas -> Window -> Rational -> Areas
alter areas win delta = case M.lookup win areas of
    Just v -> M.insert win (v * delta) areas
    Nothing -> M.insert win delta areas

-- vim: sw=4:et
