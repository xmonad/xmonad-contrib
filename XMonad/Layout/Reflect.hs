{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Reflect
-- Copyright   :  (c) Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Reflect a layout horizontally or vertically.
-----------------------------------------------------------------------------

module XMonad.Layout.Reflect (
                               -- * Usage
                               -- $usage

                               reflectHoriz, reflectVert

                             ) where

import XMonad.Core
import Graphics.X11 (Rectangle(..))
import Control.Arrow ((***), second)
import Control.Applicative ((<$>))

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.Reflect
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = reflectHoriz $ Tall 1 (3/100) (1/2)  -- put master pane on the right
--
-- 'reflectHoriz' and 'reflectVert' can be applied to any sort of layout,
-- and will simply flip the physical layout of the windows vertically or
-- horizontally.

-- | Apply a horizontal reflection (left \<--\> right) to a
--   layout.
reflectHoriz :: (LayoutClass l a) => (l a) -> Reflect l a
reflectHoriz = Reflect Horiz

-- | Apply a vertical reflection (top \<--\> bottom) to a
--   layout.
reflectVert :: (LayoutClass l a) => (l a) -> Reflect l a
reflectVert = Reflect Vert

data ReflectDir = Horiz | Vert
  deriving (Read, Show)

-- | Given an axis of reflection and the enclosing rectangle which
--   contains all the laid out windows, transform a rectangle
--   representing a window into its flipped counterpart.
reflectRect :: ReflectDir -> Rectangle -> Rectangle -> Rectangle
reflectRect Horiz (Rectangle sx _ sw _) (Rectangle rx ry rw rh) =
  Rectangle (2*sx + fi sw - rx - fi rw) ry rw rh
reflectRect Vert (Rectangle _ sy _ sh) (Rectangle rx ry rw rh) =
  Rectangle rx (2*sy + fi sh - ry - fi rh) rw rh

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


data Reflect l a = Reflect ReflectDir (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Reflect l) a where

    -- do layout l, then reflect all the generated Rectangles.
    doLayout (Reflect d l) r s = (map (second (reflectRect d r)) *** fmap (Reflect d))
                                 <$> doLayout l r s

    -- pass messages on to the underlying layout
    handleMessage (Reflect d l) = fmap (fmap (Reflect d)) . handleMessage l

    description (Reflect d l) = "Reflect" ++ xy ++ " " ++ description l
      where xy = case d of { Horiz -> "X" ; Vert -> "Y" }

