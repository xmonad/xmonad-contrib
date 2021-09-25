{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Reflect
-- Description :  Reflect a layout horizontally or vertically.
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

                               reflectHoriz, reflectVert,
                               REFLECTX(..), REFLECTY(..),
                               Reflect

                             ) where

import XMonad.Prelude (fi)
import Graphics.X11 (Rectangle(..), Window)
import Control.Arrow (second)

import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.Reflect
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = reflectHoriz $ Tall 1 (3/100) (1/2)  -- put master pane on the right
--
-- 'reflectHoriz' and 'reflectVert' can be applied to any sort of
-- layout (including Mirrored layouts) and will simply flip the
-- physical layout of the windows vertically or horizontally.
--
-- "XMonad.Layout.MultiToggle" transformers are also provided for
-- toggling layouts between reflected\/non-reflected with a keybinding.
-- To use this feature, you will also need to import the MultiToggle
-- module:
--
-- > import XMonad.Layout.MultiToggle
--
-- Next, add one or more toggles to your layout.  For example, to allow
-- separate toggling of both vertical and horizontal reflection:
--
-- > layoutHook = mkToggle (single REFLECTX) $
-- >              mkToggle (single REFLECTY) $
-- >                (tiled ||| Mirror tiled ||| ...) -- whatever layouts you use
--
-- Finally, add some keybindings to do the toggling, for example:
--
-- > , ((modm .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
-- > , ((modm .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
--

-- | Apply a horizontal reflection (left \<--\> right) to a
--   layout.
reflectHoriz :: l a -> ModifiedLayout Reflect l a
reflectHoriz = ModifiedLayout (Reflect Horiz)

-- | Apply a vertical reflection (top \<--\> bottom) to a
--   layout.
reflectVert :: l a -> ModifiedLayout Reflect l a
reflectVert = ModifiedLayout (Reflect Vert)

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



newtype Reflect a = Reflect ReflectDir deriving (Show, Read)

instance LayoutModifier Reflect a where

    -- reflect all the generated Rectangles.
    pureModifier (Reflect d) r _ wrs = (map (second $ reflectRect d r) wrs, Just $ Reflect d)

    modifierDescription (Reflect d) = "Reflect" ++ xy
      where xy = case d of { Horiz -> "X" ; Vert -> "Y" }


-------- instances for MultiToggle ------------------

data REFLECTX = REFLECTX deriving (Read, Show, Eq)
data REFLECTY = REFLECTY deriving (Read, Show, Eq)

instance Transformer REFLECTX Window where
    transform REFLECTX x k = k (reflectHoriz x) (\(ModifiedLayout _ x') -> x')

instance Transformer REFLECTY Window where
    transform REFLECTY x k = k (reflectVert x) (\(ModifiedLayout _ x') -> x')
