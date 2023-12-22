
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx
-- Description :  Advanced window decorations module for XMonad
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This set of modules contains a set of type classes and their implementations
-- which define a flexible and extensible mechanism of window decorations.
--
-- Within this mechanism, there are the following entities which define
-- how decorations will look and work:
--
-- * Main object is @DecorationEx@ layout modifier. It is from where everything
--   starts. It creates, shows and hides decoration windows (rectangles) when
--   needed. It is parameterized with decoration geometry, decoration engine and
--   theme. It calls these components to do their parts of the work.
-- * @DecorationGeometry@ defines where decoration rectangles should be placed.
--   For example, standard horizontal bar above each window; or tab bar.
-- * @DecorationEngine@ defines how decorations look and how they react on clicks.
--   Different implementations of the decoration engine can use different APIs
--   to draw decorations. Within this package, there is one implementation 
--   (@TextDecoration@), which uses plain Xlib calls, and displays decoration
--   widgets with text fragments, like @[X]@ or @[_]@. Other engines can, for
--   example, use the Cairo library to draw nice gradients and image-based widgets.
-- * A Decoration widget is an element placed on a window decoration. It defines how
--   it looks and how it responds to clicks. Examples include usual window 
--   buttons (minimize, maximize, close), window icon, window title.
-- * A Decoration theme defines colors and fonts for the decoration engine. It also
--   contains a list of decoration widgets and says where to place them (at the
--   left, at the right or in the center).
-- 
-- This mechanism makes major use of parameterized data types and type families,
-- in order to make it possible to define different types of decorations, and
-- easily combine different aspects of decorations. For example, each decoration
-- engine can be combined with each decoration geometry.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx (
  -- * Usage:
  -- $usage

  -- * Standard decoration settings
  decorationEx,
  textDecoration, textTabbed, dwmStyleDeco,
  -- * Decoration-related types
  TextDecoration (..), DefaultGeometry (..),
  TabbedGeometry (..), DwmGeometry (..),
  DecorationEx,
  -- * Theme types
  BoxBorders (..), BorderColors,
  SimpleStyle (..), GenericTheme (..),
  ThemeEx,
  -- * Widget types
  StandardCommand (..), GenericWidget (..),
  StandardWidget,
  -- * Utility functions for themes
  themeEx, borderColor, shadowBorder,
  -- * Convinience re-exports
  Shrinker (..), shrinkText,
  -- * Standard widgets
  titleW, toggleStickyW, minimizeW,
  maximizeW, closeW, dwmpromoteW,
  moveToNextGroupW, moveToPrevGroupW
  ) where

import XMonad.Layout.Decoration
import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.Geometry
import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.TextEngine
import XMonad.Layout.DecorationEx.TabbedGeometry
import XMonad.Layout.DecorationEx.DwmGeometry

-- $usage
--
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DecorationEx
-- Then edit your @layoutHook@ by adding the DwmStyle decoration to
-- your layout:
--
-- > myTheme = ThemeEx {...}
-- > myL = textDecoration shrinkText myTheme (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- This module exports only some definitions from it's submodules,
-- most likely to be used from user configurations. To define
-- your own decoration types you will likely have to import specific
-- submodules.

