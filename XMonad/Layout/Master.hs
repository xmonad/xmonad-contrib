{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Master
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that adds a distinguished master window to a base layout.
-----------------------------------------------------------------------------

module XMonad.Layout.Master (
    -- * Usage
    -- $usage
    mastered,
    Master
) where

import XMonad
import XMonad.StackSet

import Data.List
import Data.Ord

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Master
--
-- and add something like
--
-- > mastered (1/100) (1/2) $ Grid
--
-- to your layouts. This will use the left half of your screen for a master
-- window and let Grid manage the right half.
--
-- For more detailed instructions on editing the layoutHook see
-- "XMonad.Doc.Extending#Editing_the_layout_hook".
--
-- Like 'XMonad.Layout.Tall', 'Master' supports the 'XMonad.Layout.Shrink' and
-- 'XMonad.Layout.Expand' messages.

mastered :: (LayoutClass l a)
    => Rational -- ^ @delta@, the ratio of the screen to resize by
    -> Rational -- ^ @frac@, what portion of the screen to reserve for the master window
    -> l a      -- ^ the layout to use for the remaining windows
    -> Master l a
mastered d f b = Master d f' b
    where
    f' = min 1 . max 0 $ f

data Master l a =
    Master{
        delta :: Rational,
        frac :: Rational,
        base :: l a
    } deriving (Show, Read, Eq, Ord)

extractMaster :: Stack a -> (a, Maybe (Stack a))
extractMaster (Stack x ls rs) = case reverse ls of
    [] -> (x, differentiate rs)
    (m : ls') -> (m, Just $ Stack x (reverse ls') rs)

area :: Rectangle -> Dimension
area r = rect_width r * rect_height r

chop :: D -> Rectangle -> Rectangle
chop (w, h) (Rectangle rx ry rw rh) =
    let
        r' = maximumBy (comparing area)
            [ Rectangle rx (ry + fromIntegral h) rw (rh - h)
            , Rectangle (rx + fromIntegral w) ry (rw - w) rh]
    in
    r'{ rect_width = max 0 $ rect_width r', rect_height = max 0 $ rect_height r' }

instance (LayoutClass l Window) => LayoutClass (Master l) Window where
    description m = "Master " ++ description (base m)
    handleMessage m msg
        | Just Shrink <- fromMessage msg =
            return . Just $ m{ frac = max 0 $ frac m - delta m }
        | Just Expand <- fromMessage msg =
            return . Just $ m{ frac = min 1 $ frac m + delta m }
        | otherwise =
            fmap (fmap (\x -> m{ base = x })) $ handleMessage (base m) msg
    runLayout ws rect = do
        (f, ws', rect') <- case fmap extractMaster $ stack ws of
            Nothing ->
                return (id, ws, rect)
            Just (x, Nothing) -> do
                f <- mkAdjust x
                let
                    (w', h') = f (rect_width rect, rect_height rect)
                    xr = rect{ rect_width = w', rect_height = h' }
                return (((x, xr) :), ws{ stack = Nothing }, Rectangle (rect_x xr + fromIntegral w') (rect_y xr) 0 0)
            Just (x, Just st) -> do
                f <- mkAdjust x
                let
                    d@(w', h') = f (scale $ rect_width rect, rect_height rect)
                    xr = rect{ rect_width = w', rect_height = h' }
                return (((x, xr) :), ws{ stack = Just st }, chop d rect)
        (y, l) <- runLayout ws'{ layout = base m } rect'
        return (f y, fmap (\x -> m{ base = x }) l)
        where
        m = layout ws
        scale = round . (* frac m) . fromIntegral
