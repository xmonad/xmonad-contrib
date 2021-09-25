{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.AvoidFloats
-- Description :  Avoid floats when placing tiled windows.
-- Copyright   :  (c) 2014 Anders Engstrom <ankaan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  (c) Anders Engstrom <ankaan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Find a maximum empty rectangle around floating windows and use that area
-- to display non-floating windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.AvoidFloats (
                                   -- * Usage
                                   -- $usage
                                   avoidFloats,
                                   avoidFloats',
                                   AvoidFloatMsg(..),
                                   AvoidFloatItemMsg(..),
                                 ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (fi, mapMaybe, maximumBy, sortOn)
import qualified XMonad.StackSet as W

import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

-- $usage
-- You can use this module with the following in your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.AvoidFloats
--
-- and modify the layouts to call avoidFloats on the layouts where you want the
-- non-floating windows to not be behind floating windows.
--
-- > layoutHook = ... ||| avoidFloats Full ||| ...
--
-- For more detailed instructions on editing the layoutHook see:
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- Then add appropriate key bindings, for example:
--
-- > ,((modm .|. shiftMask, xK_b), sendMessage AvoidFloatToggle)
-- > ,((modm .|. controlMask, xK_b), withFocused $ sendMessage . AvoidFloatToggleItem)
-- > ,((modm .|. shiftMask .|. controlMask, xK_b), sendMessage (AvoidFloatSet False) >> sendMessage AvoidFloatClearItems)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- Note that this module is incompatible with an old way of configuring
-- "XMonad.Actions.FloatSnap". If you are having problems, please update your
-- configuration.

-- | Avoid floating windows unless the resulting area for windows would be too small.
--   In that case, use the whole screen as if this layout modifier wasn't there.
--   No windows are avoided by default, they need to be added using signals.
avoidFloats
    :: l a  -- ^ Layout to modify.
    -> ModifiedLayout AvoidFloats l a
avoidFloats = avoidFloats' 100 100 False

-- | Avoid floating windows unless the resulting area for windows would be too small.
--   In that case, use the whole screen as if this layout modifier wasn't there.
avoidFloats'
    :: Int  -- ^ Minimum width of the area used for non-floating windows.
    -> Int  -- ^ Minimum height of the area used for non-floating windows.
    -> Bool -- ^ If floating windows should be avoided by default.
    -> l a  -- ^ Layout to modify.
    -> ModifiedLayout AvoidFloats l a
avoidFloats' w h act = ModifiedLayout (AvoidFloats Nothing S.empty w h act)

data AvoidFloats a = AvoidFloats
    { cache :: Maybe ((M.Map a W.RationalRect, Rectangle), Rectangle)
    , chosen :: S.Set a
    , minw :: Int
    , minh :: Int
    , avoidAll :: Bool
    } deriving (Read, Show)

-- | Change the state of the whole avoid float layout modifier.
data AvoidFloatMsg
    = AvoidFloatToggle        -- ^ Toggle between avoiding all or only selected.
    | AvoidFloatSet Bool      -- ^ Set if all all floating windows should be avoided.
    | AvoidFloatClearItems    -- ^ Clear the set of windows to specifically avoid.

-- | Change the state of the avoid float layout modifier conserning a specific window.
data AvoidFloatItemMsg a
    = AvoidFloatAddItem a     -- ^ Add a window to always avoid.
    | AvoidFloatRemoveItem a  -- ^ Stop always avoiding selected window.
    | AvoidFloatToggleItem a  -- ^ Toggle between always avoiding selected window.

instance Message AvoidFloatMsg
instance Typeable a => Message (AvoidFloatItemMsg a)

instance LayoutModifier AvoidFloats Window where
    modifyLayoutWithUpdate lm w r = withDisplay $ \d -> do
        floating <- gets $ W.floating . windowset
        case cache lm of
            Just (key, mer) | key == (floating,r) -> (, Nothing) <$> runLayout w mer
            _ -> do rs <- io $ map toRect <$> mapM (getWindowAttributes d) (filter shouldAvoid $ M.keys floating)
                    let mer = maximumBy (comparing area) $ filter bigEnough $ maxEmptyRectangles r rs
                    (, Just $ pruneWindows $ lm { cache = Just ((floating,r),mer) }) <$> runLayout w mer
        where
            toRect :: WindowAttributes -> Rectangle
            toRect wa = let b = fi $ wa_border_width wa
                        in Rectangle (fi $ wa_x wa) (fi $ wa_y wa) (fi $ wa_width wa + 2*b) (fi $ wa_height wa + 2*b)

            bigEnough :: Rectangle -> Bool
            bigEnough rect = rect_width rect >= fi (minw lm) && rect_height rect >= fi (minh lm)

            shouldAvoid a = avoidAll lm || a `S.member` chosen lm

    pureMess lm m
        | Just AvoidFloatToggle <- fromMessage m =                                 Just $ lm { avoidAll = not (avoidAll lm), cache = Nothing }
        | Just (AvoidFloatSet s) <- fromMessage m, s /= avoidAll lm =              Just $ lm { avoidAll = s, cache = Nothing }
        | Just AvoidFloatClearItems <- fromMessage m =                             Just $ lm { chosen = S.empty, cache = Nothing }
        | Just (AvoidFloatAddItem a) <- fromMessage m, a `S.notMember` chosen lm = Just $ lm { chosen = S.insert a (chosen lm), cache = Nothing }
        | Just (AvoidFloatRemoveItem a) <- fromMessage m, a `S.member` chosen lm = Just $ lm { chosen = S.delete a (chosen lm), cache = Nothing }
        | Just (AvoidFloatToggleItem a) <- fromMessage m =                         let op = if a `S.member` chosen lm then S.delete else S.insert
                                                                                   in Just $ lm { chosen = op a (chosen lm), cache = Nothing }
        | otherwise =                                                              Nothing

pruneWindows :: AvoidFloats Window -> AvoidFloats Window
pruneWindows lm = case cache lm of
    Nothing -> lm
    Just ((floating,_),_) -> lm { chosen = S.filter (`M.member` floating) (chosen lm) }

-- | Find all maximum empty rectangles (MERs) that are axis aligned. This is
--   done in O(n^2) time using a modified version of the algoprithm MERAlg 1
--   described in \"On the maximum empty rectangle problem\" by A. Naamad, D.T.
--   Lee and W.-L HSU. Published in Discrete Applied Mathematics 8 (1984.)
maxEmptyRectangles :: Rectangle -> [Rectangle] -> [Rectangle]
maxEmptyRectangles br rectangles = filter (\a -> area a > 0) $ upAndDownEdge ++ noneOrUpEdge ++ downEdge
    where
        upAndDownEdge = findGaps br rectangles
        noneOrUpEdge = concatMap (everyLower br bottoms) bottoms
        downEdge = mapMaybe (bottomEdge br bottoms) bottoms
        bottoms = sortOn bottom $ splitContainers rectangles

everyLower :: Rectangle -> [Rectangle] -> Rectangle -> [Rectangle]
everyLower br bottoms r = let (rs, boundLeft, boundRight, boundRects) = foldr (everyUpper r) ([], left br, right br, reverse bottoms) bottoms
                              (boundLeft', boundRight', _) = shrinkBounds boundLeft boundRight boundRects r (top br)
                          in mkRect boundLeft' boundRight' (top br) (top r) ?: rs

everyUpper
    :: Rectangle                         -- ^ The current rectangle where the top edge is used.
    -> Rectangle                         -- ^ The current rectangle where the bottom edge is used.
    -> ([Rectangle],Int,Int,[Rectangle]) -- ^ List of MERs found so far, left bound, right bound and list of rectangles used for bounds.
    -> ([Rectangle],Int,Int,[Rectangle])
everyUpper lower upper (rs, boundLeft, boundRight, boundRects) = (r?:rs, boundLeft', boundRight', boundRects')
    where
        r = mkRect boundLeft' boundRight' (bottom upper) (top lower)
        (boundLeft', boundRight', boundRects') = shrinkBounds boundLeft boundRight boundRects lower (bottom upper)

shrinkBounds :: Int -> Int -> [Rectangle] -> Rectangle -> Int -> (Int, Int, [Rectangle])
shrinkBounds boundLeft boundRight boundRects lower upperLimit = (boundLeft', boundRight', boundRects')
    where
        (shrinkers, boundRects') = span (\a -> bottom a > upperLimit) boundRects
        (boundLeft', boundRight') = foldr (shrinkBounds' lower) (boundLeft, boundRight) $ filter (\a -> top a < top lower) shrinkers

shrinkBounds' :: Rectangle -> Rectangle -> (Int, Int) -> (Int, Int)
shrinkBounds' mr r (boundLeft, boundRight)
    | right r < right mr = (max boundLeft $ right r, boundRight)
    | left r > left mr = (boundLeft, min boundRight $ left r)
    | otherwise = (right r, left r) -- r is horizontally covering all of mr; make sure the area of this rectangle will always be 0.

bottomEdge :: Rectangle -> [Rectangle] -> Rectangle -> Maybe Rectangle
bottomEdge br bottoms r = let rs = filter (\a -> bottom r < bottom a && top a < bottom br) bottoms
                              boundLeft = maximum $ left br : filter (< right r) (map right rs)
                              boundRight = minimum $ right br : filter (> left r) (map left rs)
                          in if any (\a -> left a <= left r && right r <= right a) rs
                             then Nothing
                             else mkRect boundLeft boundRight (bottom r) (bottom br)

-- | Split rectangles that horizontally fully contains another rectangle
--   without sharing either the left or right side.
splitContainers :: [Rectangle] -> [Rectangle]
splitContainers rects = splitContainers' [] $ sortOn rect_width rects
    where
        splitContainers' :: [Rectangle] -> [Rectangle] -> [Rectangle]
        splitContainers' res [] = res
        splitContainers' res (r:rs) = splitContainers' (r:res) $ concatMap (doSplit r) rs

        doSplit :: Rectangle -> Rectangle -> [Rectangle]
        doSplit guide r
            | left guide <= left r || right r <= right guide = [r]
            | otherwise = let w0 = fi (rect_x guide - rect_x r) + (rect_width guide `div` 2)
                              w1 = rect_width r - w0
                          in  [ Rectangle (rect_x r)          (rect_y r) w0 (rect_height r)
                              , Rectangle (rect_x r + fi w0)  (rect_y r) w1 (rect_height r)
                              ]

-- | Find all horizontal gaps that are left empty from top to bottom of screen.
findGaps
    :: Rectangle    -- ^ Bounding rectangle.
    -> [Rectangle]  -- ^ List of all rectangles that can cover areas in the bounding rectangle.
    -> [Rectangle]
findGaps br rs = let (gaps,end) = foldr findGaps' ([], left br) $ sortOn (Down . left) $ filter inBounds rs
                     lastgap = mkRect end (right br) (top br) (bottom br)
                 in lastgap?:gaps
    where
        findGaps' :: Rectangle -> ([Rectangle], Int) -> ([Rectangle], Int)
        findGaps' r (gaps, end) = let gap = mkRect end (left r) (top br) (bottom br)
                                  in (gap?:gaps, max end (right r))

        inBounds :: Rectangle -> Bool
        inBounds r = left r < right br && left br < right r

(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x:xs
_ ?: xs = xs

left, right, top, bottom, area :: Rectangle -> Int
left r = fi (rect_x r)
right r = fi (rect_x r) + fi (rect_width r)
top r = fi (rect_y r)
bottom r = fi (rect_y r) + fi (rect_height r)
area r = fi (rect_width r * rect_height r)

mkRect :: Int -> Int -> Int -> Int -> Maybe Rectangle
mkRect l r t b = let rect = Rectangle (fi l) (fi t) (fi $ max 0 $ r-l) (fi $ max 0 $ b-t)
                 in if area rect > 0
                    then Just rect
                    else Nothing
