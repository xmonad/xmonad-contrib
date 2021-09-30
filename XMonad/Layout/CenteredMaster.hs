{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CenteredMaster
-- Description :  Place the master pane on top of other windows; in the center or top right.
-- Copyright   :  (c) 2009 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Two layout modifiers. centerMaster places master window at center,
-- on top of all other windows, which are managed by base layout.
-- topRightMaster is similar, but places master window in top right corner
-- instead of center.
--
-----------------------------------------------------------------------------

module XMonad.Layout.CenteredMaster (
         -- * Usage
         -- $usage

         centerMaster,
         topRightMaster,
         CenteredMaster, TopRightMaster,
         ) where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import Control.Arrow (first)

-- $usage
-- This module defines two new layout modifiers: centerMaster and topRightMaster.
-- centerMaster places master window at center of screen, on top of others.
-- All other windows in background are managed by base layout.
-- topRightMaster is like centerMaster, but places master window in top right corner instead of center.
--
-- Yo can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.CenteredMaster
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = centerMaster Grid ||| ...

-- | Function that decides where master window should be placed
type Positioner = Rectangle -> Rectangle

-- | Data type for LayoutModifier
data CenteredMaster a = CenteredMaster deriving (Read,Show)

instance LayoutModifier CenteredMaster Window where
  modifyLayout CenteredMaster = applyPosition (center (5/7) (5/7))

data TopRightMaster a = TopRightMaster deriving (Read,Show)

instance LayoutModifier TopRightMaster Window where
  modifyLayout TopRightMaster = applyPosition (topRight (3/7) (1/2))

-- | Modifier that puts master window in center, other windows in background
-- are managed by given layout
centerMaster :: LayoutClass l a => l a -> ModifiedLayout CenteredMaster l a
centerMaster = ModifiedLayout CenteredMaster

-- | Modifier that puts master window in top right corner, other windows in background
-- are managed by given layout
topRightMaster :: LayoutClass l a => l a -> ModifiedLayout TopRightMaster l a
topRightMaster = ModifiedLayout TopRightMaster

-- | Internal function, doing main job
applyPosition :: (LayoutClass l a, Eq a) =>
                    Positioner
                 -> W.Workspace WorkspaceId (l a) a
                 -> Rectangle
                 -> X ([(a, Rectangle)], Maybe (l a))

applyPosition pos wksp rect = do
  let stack = W.stack wksp
  let ws = W.integrate' stack
  if null ws then
     runLayout wksp rect
     else do
       let firstW = head ws
       let other  = tail ws
       let filtStack = stack >>= W.filter (firstW /=)
       wrs <- runLayout (wksp {W.stack = filtStack}) rect
       return $ first ((firstW, place pos other rect) :) wrs

-- | Place master window (it's Rectangle is given), using the given Positioner.
-- If second argument is empty (that is, there is only one window on workspace),
-- place that window fullscreen.
place :: Positioner -> [a] -> Rectangle -> Rectangle
place _ [] rect = rect
place pos _ rect = pos rect

-- | Function that calculates Rectangle at top right corner of given Rectangle
topRight :: Float -> Float -> Rectangle -> Rectangle
topRight rx ry (Rectangle sx sy sw sh) = Rectangle x sy w h
  where w = round (fromIntegral sw * rx)
        h = round (fromIntegral sh * ry)
        x = sx + fromIntegral (sw-w)

-- | Function that calculates Rectangle at center of given Rectangle.
center :: Float -> Float -> Rectangle -> Rectangle
center rx ry (Rectangle sx sy sw sh) = Rectangle x y w h
  where w = round (fromIntegral sw * rx)
        h = round (fromIntegral sh * ry)
        x = sx + fromIntegral (sw-w) `div` 2
        y = sy + fromIntegral (sh-h) `div` 2
