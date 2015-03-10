{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Spacing
-- Copyright   :  (c) Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- Add a configurable amount of space around windows.
-----------------------------------------------------------------------------

module XMonad.Layout.Spacing (
                               -- * Usage
                               -- $usage

                               spacing, Spacing,
                               spacingWithEdge, SpacingWithEdge,
                               smartSpacing, SmartSpacing,
                               smartSpacingWithEdge, SmartSpacingWithEdge,
                               SpacingMsg(..)
                             ) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Core (runLayout,Message,fromMessage,Typeable)
import XMonad.StackSet (up, down, Workspace(..))
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.Spacing
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = spacing 2 $ Tall 1 (3/100) (1/2)
-- >                      -- put a 2px space around every window
--

-- | Surround all windows by a certain number of pixels of blank space.
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing p = ModifiedLayout (Spacing p)

data Spacing a = Spacing Int deriving (Show, Read)

-- | Message to dynamically increase, decrease or set the size of the window spacing
data SpacingMsg = SetSpacing Int | IncSpacing Int deriving (Show,Read,Eq,Typeable)
instance Message SpacingMsg

instance LayoutModifier Spacing a where

    pureModifier (Spacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    pureMess (Spacing px) m
     | Just (SetSpacing px') <- fromMessage m = Just $ Spacing (max 0 px')
     | Just (IncSpacing n)   <- fromMessage m = Just $ Spacing (max 0 (px+n))
     | otherwise = Nothing

    modifierDescription (Spacing p) = "Spacing " ++ show p

-- | Surround all windows by a certain number of pixels of blank space, and
-- additionally adds the same amount of spacing around the edge of the screen.
spacingWithEdge :: Int -> l a -> ModifiedLayout SpacingWithEdge l a
spacingWithEdge p = ModifiedLayout (SpacingWithEdge p)

data SpacingWithEdge a = SpacingWithEdge Int deriving (Show, Read)

instance LayoutModifier SpacingWithEdge a where

    pureModifier (SpacingWithEdge p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    pureMess (SpacingWithEdge px) m
     | Just (SetSpacing px') <- fromMessage m = Just $ SpacingWithEdge (max 0 px')
     | Just (IncSpacing n)   <- fromMessage m = Just $ SpacingWithEdge (max 0 (px+n))
     | otherwise = Nothing

    modifyLayout (SpacingWithEdge p) w r = runLayout w (shrinkRect p r)

    modifierDescription (SpacingWithEdge p) = "SpacingWithEdge " ++ show p

shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y+fi p) (w-2*fi p) (h-2*fi p)

-- | Surrounds all windows with blank space, except when the window is the only
-- visible window on the current workspace.
smartSpacing :: Int -> l a -> ModifiedLayout SmartSpacing l a
smartSpacing p = ModifiedLayout (SmartSpacing p)

data SmartSpacing a = SmartSpacing Int deriving (Show, Read)

instance LayoutModifier SmartSpacing a where

    pureModifier _ _ _ [x] = ([x], Nothing)
    pureModifier (SmartSpacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifierDescription (SmartSpacing p) = "SmartSpacing " ++ show p

-- | Surrounds all windows with blank space, and adds the same amount of spacing
-- around the edge of the screen, except when the window is the only visible
-- window on the current workspace.
smartSpacingWithEdge :: Int -> l a -> ModifiedLayout SmartSpacingWithEdge l a
smartSpacingWithEdge p = ModifiedLayout (SmartSpacingWithEdge p)

data SmartSpacingWithEdge a = SmartSpacingWithEdge Int deriving (Show, Read)

instance LayoutModifier SmartSpacingWithEdge a where

    pureModifier _ _ _ [x] = ([x], Nothing)
    pureModifier (SmartSpacingWithEdge p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifyLayout (SmartSpacingWithEdge p) w r
        | maybe False (\s -> null (up s) && null (down s)) (stack w) = runLayout w r
        | otherwise = runLayout w (shrinkRect p r)

    modifierDescription (SmartSpacingWithEdge p) = "SmartSpacingWithEdge " ++ show p
