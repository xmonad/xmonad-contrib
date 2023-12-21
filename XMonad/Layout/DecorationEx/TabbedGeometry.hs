{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.TabbedGeometry
-- Description :  Tab-based window decoration geometry
-- Copyright   :  (c) 2007 Andrea Rossato, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module defines window decoration geometry based on tabs.
-- The tabs can follow horizontally and be placed above or below windows;
-- in such case, tabs can occupy full width of the window or be aligned to
-- left or right. Or tabs can go vertically near left or right side of
-- the window.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.TabbedGeometry (
    textTabbed,
    TabbedGeometry (..),
    HorizontalTabPlacement (..),
    VerticalTabPlacement (..),
    HorizontalTabWidth (..),
    HorizontalTabsAlignment (..),
    SingleTabMode (..)
  ) where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Geometry
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

-- | Placement of tabs when they go horizontally:
-- should they be placed above or below the window.
data HorizontalTabPlacement = Top | Bottom
  deriving (Eq, Read, Show)

-- | Placement of tabs when they go vertically:
-- should they appear at left or at right side of the window.
data VerticalTabPlacement = TabsAtLeft | TabsAtRight
  deriving (Eq, Read, Show)

-- | Width of tabs when they go horizontally.
data HorizontalTabWidth =
      AutoWidth             -- ^ Define the width automatically by evenly dividing windows' width
    | FixedWidth !Dimension -- ^ Use fixed width of the tab
  deriving (Eq, Read, Show)

-- | Alignment of tabs when they go horizontally.
data HorizontalTabsAlignment = AlignTabsLeft | AlignTabsCenter | AlignTabsRight
  deriving (Eq, Read, Show)

-- | What to do if there is only one tab.
data SingleTabMode = ShowTab | HideTab
  deriving (Eq, Read, Show)

data TabbedGeometry a =
      HorizontalTabs {
          showIfSingleWindow :: !SingleTabMode      -- ^ What to do if there is only one tab
        , hTabPlacement :: !HorizontalTabPlacement  -- ^ Where to place horizontal tabs
        , hTabAlignment :: !HorizontalTabsAlignment -- ^ How to align horizontal tabs (makes sense with fixed width of tabs).
        , hTabWidth :: !HorizontalTabWidth          -- ^ Width of horizontal tabs
        , hTabHeight :: !Dimension                  -- ^ Height of horizontal tabs
      }
    | VerticalTabs {
          showIfSingleWindow :: !SingleTabMode      -- ^ What to do if there is only one tab
        , vTabPlacement :: !VerticalTabPlacement    -- ^ Where to place vertical tabs
        , vTabWidth :: !Dimension                   -- ^ Width of vertical tabs
        , vTabHeight :: !Dimension                  -- ^ Height of vertical tabs
      }
  deriving (Show, Read)

instance Default (TabbedGeometry a) where
  def = HorizontalTabs ShowTab Top AlignTabsLeft AutoWidth 20 

instance DecorationGeometry TabbedGeometry Window where

  describeGeometry _ = "Tabbed"

  pureDecoration tabs _ stack wrs (window, windowRect) =
    let Rectangle windowX windowY windowWidth windowHeight = windowRect
        -- windows that are mapped onto the same rectangle as current one are considered to
        -- be in one tabs group
        tabbedWindows = filter (`elem` map fst (filter ((==windowRect) . snd) wrs)) (W.integrate stack)
        mbWindowIndex = window `elemIndex` tabbedWindows
        numWindows = length tabbedWindows
    in  if numWindows > 1 || (showIfSingleWindow tabs == ShowTab && numWindows > 0)
          then
            case tabs of
              HorizontalTabs {..} ->
                  Just $ case hTabPlacement of
                            Top    -> Rectangle decoX windowY effectiveTabWidth hTabHeight
                            Bottom -> Rectangle decoX (windowY + fi (windowHeight - hTabHeight)) effectiveTabWidth hTabHeight
                where
                  decoX = maybe windowX tabX mbWindowIndex

                  -- If there are too many windows or configured tab width
                  -- is too big, then we have to switch to 'auto' mode.
                  hTabWidth' =
                    case hTabWidth of
                      AutoWidth -> AutoWidth
                      FixedWidth tabWidth
                        | tabWidth * fi numWindows > windowWidth -> AutoWidth
                        | otherwise -> FixedWidth tabWidth

                  effectiveTabWidth =
                    case hTabWidth' of
                      AutoWidth -> fi $ maybe windowX (\i -> tabX (i+1) - tabX i) mbWindowIndex
                      FixedWidth tabWidth -> tabWidth

                  allTabsWidth =
                    case hTabWidth' of
                      AutoWidth -> fi windowWidth
                      FixedWidth _ -> fi $ min windowWidth $ effectiveTabWidth * max 1 (fi numWindows)

                  tabsStartX =
                    case hTabAlignment of
                      AlignTabsLeft -> windowX
                      AlignTabsRight -> windowX + fi windowWidth - allTabsWidth
                      AlignTabsCenter -> windowX + (fi windowWidth - allTabsWidth) `div` 2

                  -- X coordinate of i'th window in horizontal tabs layout
                  tabX i = tabsStartX +
                        case hTabWidth' of
                          AutoWidth -> fi ((windowWidth * fi i) `div` max 1 (fi numWindows))
                          FixedWidth _ -> fi effectiveTabWidth * fi i

              VerticalTabs {..} ->
                  Just $ case vTabPlacement of
                            TabsAtLeft  -> fixHeightTab windowX
                            TabsAtRight -> fixHeightTab (windowX + fi (windowWidth - vTabWidth))
                where
                  fixHeightLoc i = windowY + fi vTabHeight * fi i
                  fixHeightTab x = Rectangle x
                        (maybe windowY fixHeightLoc mbWindowIndex) vTabWidth vTabHeight
          else Nothing

  shrinkWindow tabs (Rectangle _ _ dw dh) (Rectangle x y w h) =
    case tabs of
      HorizontalTabs {..} ->
        case hTabPlacement of
            Top -> Rectangle x (y + fi dh) w (h - dh)
            Bottom -> Rectangle x y w (h - dh)
      VerticalTabs {..} ->
        case vTabPlacement of
            TabsAtLeft  -> Rectangle (x + fi dw) y (w - dw) h
            TabsAtRight -> Rectangle x y (w - dw) h

-- | Add tabbed decorations (with default settings) with text-based widgets to a layout.
textTabbed :: (Shrinker shrinker)
           => shrinker               -- ^ Strings shrinker, e.g. @shrinkText@
           -> ThemeEx StandardWidget -- ^ Decoration theme
           -> l Window               -- ^ Layout to be decorated
           -> ModifiedLayout (DecorationEx TextDecoration StandardWidget TabbedGeometry shrinker) l Window
textTabbed shrinker theme = decorationEx shrinker theme TextDecoration def

