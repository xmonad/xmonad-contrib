{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MosaicAlt
-- Description :  An alternative version of "XMonad.Layout.Mosaic".
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

module XMonad.Layout.MosaicAlt (
        -- * Usage:
        -- $usage
        MosaicAlt(..)
        , shrinkWindowAlt
        , expandWindowAlt
        , tallWindowAlt
        , wideWindowAlt
        , resetAlt

        , Params, Param
        , HandleWindowAlt
    ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Prelude ( sortBy )
import Data.Ratio

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MosaicAlt
-- > import qualified Data.Map as M
--
-- Then edit your @layoutHook@ by adding the MosaicAlt layout:
--
-- > myLayout = MosaicAlt M.empty ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >     , ((modm .|. shiftMask  , xK_a    ), withFocused (sendMessage . expandWindowAlt))
-- >     , ((modm .|. shiftMask  , xK_z    ), withFocused (sendMessage . shrinkWindowAlt))
-- >     , ((modm .|. shiftMask  , xK_s    ), withFocused (sendMessage . tallWindowAlt))
-- >     , ((modm .|. shiftMask  , xK_d    ), withFocused (sendMessage . wideWindowAlt))
-- >     , ((modm .|. controlMask, xK_space), sendMessage resetAlt)
-- >     ...
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data HandleWindowAlt =
    ShrinkWindowAlt Window
    | ExpandWindowAlt Window
    | TallWindowAlt Window
    | WideWindowAlt Window
    | ResetAlt
    deriving ( Eq )
instance Message HandleWindowAlt
shrinkWindowAlt, expandWindowAlt :: Window -> HandleWindowAlt
tallWindowAlt, wideWindowAlt :: Window -> HandleWindowAlt
shrinkWindowAlt = ShrinkWindowAlt
expandWindowAlt = ExpandWindowAlt
tallWindowAlt = TallWindowAlt
wideWindowAlt = WideWindowAlt
resetAlt :: HandleWindowAlt
resetAlt = ResetAlt

data Param = Param { area, aspect :: Rational } deriving ( Show, Read )
type Params = M.Map Window Param
newtype MosaicAlt a = MosaicAlt Params deriving ( Show, Read )

instance LayoutClass MosaicAlt Window where
    description _ = "MosaicAlt"
    doLayout (MosaicAlt params) rect stack =
            return (arrange rect stack params', Just $ MosaicAlt params')
        where
            params' = ins (W.up stack) $ ins (W.down stack) $ ins [W.focus stack] params
            ins wins as = foldl M.union as $ map (`M.singleton` Param 1 1.5) wins

    handleMessage (MosaicAlt params) msg = return $ case fromMessage msg of
        Just (ShrinkWindowAlt w) -> Just $ MosaicAlt $ alter params w (4 % 5) 1
        Just (ExpandWindowAlt w) -> Just $ MosaicAlt $ alter params w (6 % 5) 1
        Just (TallWindowAlt w) -> Just $ MosaicAlt $ alter params w 1 (3 % 4)
        Just (WideWindowAlt w) -> Just $ MosaicAlt $ alter params w 1 (5 % 4)
        Just ResetAlt -> Just $ MosaicAlt M.empty
        _ -> Nothing

-- Change requested params for a window.
alter :: Params -> Window -> Rational -> Rational -> Params
alter params win arDelta asDelta = case M.lookup win params of
    Just (Param ar as) -> M.insert win (Param (ar * arDelta) (as * asDelta)) params
    Nothing -> M.insert win (Param arDelta (1.5 * asDelta)) params

-- Layout algorithm entry point.
arrange :: Rectangle -> W.Stack Window -> Params -> [(Window, Rectangle)]
arrange rect stack params = r
    where
        (_, r) = findSplits 3 rect tree params
        tree = makeTree (sortBy areaCompare wins) params
        wins = reverse (W.up stack) ++ W.focus stack : W.down stack
        areaCompare a b = or1 b `compare` or1 a
        or1 w = maybe 1 area $ M.lookup w params

-- Recursively group windows into a binary tree. Aim to balance the tree
-- according to the total requested area in each branch.
data Tree = Node (Rational, Tree) (Rational, Tree) | Leaf Window | None
makeTree :: [Window] -> Params -> Tree
makeTree wins params = case wins of
    [] -> None
    [x] -> Leaf x
    _ -> Node (aArea, makeTree aWins params) (bArea, makeTree bWins params)
        where ((aWins, aArea), (bWins, bArea)) = areaSplit params wins

-- Split a list of windows in half by area.
areaSplit :: Params -> [Window] -> (([Window], Rational), ([Window], Rational))
areaSplit params = gather [] 0 [] 0
    where
        gather a aa b ba (r : rs) =
            if aa <= ba
                then gather (r : a) (aa + or1 r) b ba rs
                else gather a aa (r : b) (ba + or1 r) rs
        gather a aa b ba [] = ((reverse a, aa), (b, ba))
        or1 w = maybe 1 area $ M.lookup w params

-- Figure out which ways to split the space, by exhaustive search.
-- Complexity is quadratic in the number of windows.
findSplits :: Int -> Rectangle -> Tree -> Params -> (Double, [(Window, Rectangle)])
findSplits _ _ None _ = (0, [])
findSplits _ rect (Leaf w) params = (aspectBadness rect w params, [(w, rect)])
findSplits depth rect (Node (aArea, aTree) (bArea, bTree)) params =
        if hBadness < vBadness then (hBadness, hList) else (vBadness, vList)
    where
        (hBadness, hList) = trySplit splitHorizontallyBy
        (vBadness, vList) = trySplit splitVerticallyBy
        trySplit splitBy =
                (aBadness + bBadness, aList ++ bList)
            where
                (aBadness, aList) = findSplits (depth - 1) aRect aTree params
                (bBadness, bList) = findSplits (depth - 1) bRect bTree params
                (aRect, bRect) = splitBy ratio rect
        ratio = aArea / (aArea + bArea)

-- Decide how much we like this rectangle.
aspectBadness :: Rectangle -> Window -> Params -> Double
aspectBadness rect win params =
        (if a < 1 then tall else wide) * sqrt(w * h)
    where
        tall = if w < 700 then (1 / a) * (700 / w) else 1 / a
        wide = if w < 700 then a else a * w / 700
        a = (w / h) / fromRational (maybe 1.5 aspect $ M.lookup win params)
        w = fromIntegral $ rect_width rect
        h = fromIntegral $ rect_height rect

-- vim: sw=4:et
