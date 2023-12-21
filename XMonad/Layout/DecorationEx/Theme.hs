{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.Theme
-- Description :  Utility functions to deal with decoration themes.
-- Copyright   :  2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module exposes utility function to convert Theme type from
-- "XMonad.Layout.Decoration" to theme used by @TextDecoration@.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.Theme (
    themeEx,
    borderColor,
    shadowBorder
  ) where

import qualified Data.Map as M

import XMonad
import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Widgets

-- | Convert Theme type from "XMonad.Layout.Decoration" to 
-- theme type used by "XMonad.Layout.DecorationEx.TextEngine".
themeEx :: D.Theme -> ThemeEx StandardWidget
themeEx t =
    GenericTheme {
          exActive = SimpleStyle (D.activeColor t) (D.activeTextColor t) (D.activeColor t) (D.activeBorderWidth t) (borderColor $ D.activeColor t)
        , exInactive = SimpleStyle (D.inactiveColor t) (D.inactiveTextColor t) (D.inactiveColor t) (D.inactiveBorderWidth t) (borderColor $ D.inactiveColor t)
        , exUrgent = SimpleStyle (D.urgentColor t) (D.urgentTextColor t) (D.urgentColor t) (D.urgentBorderWidth t) (borderColor $ D.urgentColor t)
        , exPadding = BoxBorders 0 4 0 4
        , exFontName = D.fontName t
        , exOnDecoClick = M.fromList [(1, FocusWindow)]
        , exDragWindowButtons = [1]
        , exWidgetsLeft = []
        , exWidgetsCenter = []
        , exWidgetsRight = []
      }

instance Default (ThemeEx StandardWidget) where
  def = themeEx (def :: D.Theme)

borderColor :: String -> BorderColors
borderColor c = BoxBorders c c c c

shadowBorder :: String -> String -> BorderColors
shadowBorder highlight shadow = BoxBorders highlight shadow shadow highlight

