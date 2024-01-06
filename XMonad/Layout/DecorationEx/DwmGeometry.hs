{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.DwmGeometry
-- Description :  DWM-style window decoration geometry
-- Copyright   :  (c) 2007 Andrea Rossato, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This defines window decorations which are shown as a bar of fixed width
-- on top of window.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.DwmGeometry (
    -- * Usage:
    -- $usage
    DwmGeometry (..),
    dwmStyleDeco, dwmStyleDecoEx
  ) where 

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Geometry
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

-- $usage
-- You can use this module with the following in your
-- @xmonad.hs@:
--
-- > import XMonad.Layout.DecorationEx.DwmStyle
-- Then edit your @layoutHook@ by adding the DwmStyle decoration to
-- your layout:
--
-- > myL = dwmStyleDeco shrinkText (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Decoration geometry data type
data DwmGeometry a = DwmGeometry {
      dwmShowForFocused :: !Bool         -- ^ Whether to show decorations on focused windows
    , dwmHorizontalPosition :: !Rational -- ^ Horizontal position of decoration rectangle.
                                         -- 0 means place it at left corner, 1 - place it at
                                         -- right corner, @1%2@ - place it at center.
    , dwmDecoHeight :: !Dimension        -- ^ Height of decoration rectangle
    , dwmDecoWidth :: !Dimension         -- ^ Width of decoration rectangle
  }
  deriving (Show, Read)

instance Default (DwmGeometry a) where
  def = DwmGeometry False 1 20 200

instance DecorationGeometry DwmGeometry Window where
  describeGeometry _ = "DwmStyle"

  pureDecoration (DwmGeometry {..}) _ stack _ (w, Rectangle x y windowWidth _) =
    let width = min windowWidth dwmDecoWidth
        halfWidth = width `div` 2
        minCenterX = x + fi halfWidth
        maxCenterX = x + fi windowWidth - fromIntegral halfWidth
        centerX = round ((1 - dwmHorizontalPosition)*fi minCenterX + dwmHorizontalPosition*fi maxCenterX) :: Position
        decoX = centerX - fi halfWidth
        focusedWindow = W.focus stack
        isFocused = focusedWindow == w
    in  if (not dwmShowForFocused && isFocused) || not (D.isInStack stack w)
          then Nothing
          else Just $ Rectangle decoX y width dwmDecoHeight

  shrinkWindow _ _ windowRect = windowRect

-- | Add a decoration to window layout. Widgets are indicated with text fragments using TextDecoration;
-- decoration placement can be adjusted.
dwmStyleDecoEx :: D.Shrinker shrinker    
             => shrinker               -- ^ Strings shrinker, for example @shrinkText@
             -> DwmGeometry Window
             -> ThemeEx StandardWidget -- ^ Decoration theme (font, colors, widgets, etc)
             -> l Window               -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget DwmGeometry shrinker) l Window
dwmStyleDecoEx shrinker geom theme = decorationEx shrinker theme TextDecoration geom

-- | Add a decoration to window layout. Widgets are indicated with text fragments using TextDecoration;
-- decoration placement is similar to DWM.
dwmStyleDeco :: D.Shrinker shrinker    
             => shrinker               -- ^ Strings shrinker, for example @shrinkText@
             -> ThemeEx StandardWidget -- ^ Decoration theme (font, colors, widgets, etc)
             -> l Window               -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget DwmGeometry shrinker) l Window
dwmStyleDeco shrinker = dwmStyleDecoEx shrinker def

