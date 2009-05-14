{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

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

                               spacing

                             ) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)

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

instance LayoutModifier Spacing a where

    pureModifier (Spacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifierDescription (Spacing p) = "Spacing " ++ show p

shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y+fi p) (w-2*fi p) (h-2*fi p)
  where fi n = fromIntegral n   -- avoid the DMR
