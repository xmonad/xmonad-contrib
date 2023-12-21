{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.Geometry
-- Description :  Type class which is responsible for defining the placement
--                of window decorations
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module defines @DecorationGeometry@ type class, and default implementation for it.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.Geometry (
    DecorationGeometry (..),
    DefaultGeometry (..)
  ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.Decoration as D

-- | Decoration geometry class.
-- Decoration geometyr is responsible for placement of window decorations: whether
-- they should be on the top of the window or on the bottom, should they go for 
-- full window widht or only be of certain width, etc.
-- This does not know what will be drawn inside decorations.
class (Read (geom a), Show (geom a),
       Eq a)
    => DecorationGeometry geom a where

    -- | Give a name to decoration geometry implementation.
    describeGeometry :: geom a -> String

    -- | Reduce original window size to make space for decoration, if necessary.
    shrinkWindow :: geom a -> Rectangle -> Rectangle -> Rectangle
    shrinkWindow _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    -- | The pure version of the main method, 'decorate'.
    -- The method should return a rectangle where to place window decoration,
    -- or Nothing if this window is not to be decorated.
    pureDecoration :: geom a          -- ^ Decoration geometry instance
                   -> Rectangle       -- ^ Screen rectangle
                   -> W.Stack a       -- ^ Current stack of windows being displayed
                   -> [(a,Rectangle)] -- ^ Set of all windows with their corresponding rectangle
                   -> (a,Rectangle)   -- ^ Window being decorated and it's rectangle
                   -> Maybe Rectangle

    -- | The method should return a rectangle where to place window decoration,
    -- or Nothing if this window is not to be decorated.
    decorateWindow :: geom a           -- ^ Decoration geometry instance
                   -> Rectangle        -- ^ Screen rectangle
                   -> W.Stack a        -- ^ Current stack of windows being displayed
                   -> [(a, Rectangle)] -- ^ Set of all windows with their corresponding rectangle
                   -> (a, Rectangle)   -- ^ Window being decorated and it's rectangle
                   -> X (Maybe Rectangle)
    decorateWindow geom r s wrs wr = return $ pureDecoration geom r s wrs wr

-- | Data type for default implementation of DecorationGeometry.
-- This defines simple decorations: a horizontal bar at the top of each window,
-- running for full width of the window.
newtype DefaultGeometry a = DefaultGeometry {
    gDecorationHeight :: Dimension
  }
  deriving (Read, Show)

instance Eq a => DecorationGeometry DefaultGeometry a where
  describeGeometry _ = "Default"

  pureDecoration (DefaultGeometry {..}) _ s _ (w, Rectangle x y windowWidth windowHeight) =
      if D.isInStack s w && (gDecorationHeight < windowHeight)
        then Just $ Rectangle x y windowWidth gDecorationHeight
        else Nothing

instance Default (DefaultGeometry a) where
  def = DefaultGeometry 20

