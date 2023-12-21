{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.TextEngine
-- Description :  Text-based window decoration engine
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- Window decoration engine, that uses text fragments (like @"[X]"@) to indicate
-- widgets (window buttons).
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.TextEngine (
    textDecoration,
    TextDecoration (..)
  ) where 

import qualified Data.Map as M

import XMonad
import XMonad.Prelude
import XMonad.Layout.LayoutModifier
import XMonad.Util.Font

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Engine
import XMonad.Layout.DecorationEx.Geometry
import XMonad.Layout.DecorationEx.Widgets

-- | Decoration engine data type
data TextDecoration widget a = TextDecoration
  deriving (Show, Read)

instance ClickHandler (GenericTheme SimpleStyle) StandardWidget where
  onDecorationClick theme button = M.lookup button (exOnDecoClick theme)
  isDraggingEnabled theme button = button `elem` exDragWindowButtons theme

instance (TextWidget widget, ClickHandler (GenericTheme SimpleStyle) widget)
  => DecorationEngine TextDecoration widget Window where
  type Theme TextDecoration = GenericTheme SimpleStyle
  type DecorationPaintingContext TextDecoration = XPaintingContext
  type DecorationEngineState TextDecoration = XMonadFont

  describeEngine _ = "TextDecoration"

  calcWidgetPlace = calcTextWidgetPlace

  paintWidget = paintTextWidget

  paintDecoration = paintDecorationSimple

  initializeState engine geom theme = initXMF (themeFontName theme)
  releaseStateResources engine = releaseXMF

-- | Implementation of @paintWidget@ for decoration engines based on @TextDecoration@.
paintTextWidget :: (TextWidget widget,
                    Style (Theme engine widget) ~ SimpleStyle,
                    DecorationPaintingContext engine ~ XPaintingContext,
                    DecorationEngineState engine ~ XMonadFont,
                    Shrinker shrinker,
                    DecorationEngine engine widget Window)
                => engine widget Window
                -> DecorationPaintingContext engine
                -> WidgetPlace
                -> shrinker
                -> DrawData engine widget
                -> widget
                -> Bool
                -> X ()
paintTextWidget engine (dpy, pixmap, gc) place shrinker dd widget isExpose = do
    let style = ddStyle dd
        rect = wpRectangle place
        x = rect_x rect
        y = wpTextYPosition place
    str <- widgetString dd widget
    str' <- if isShrinkable widget
              then getShrinkedWindowName engine shrinker (ddEngineState dd) str (rect_width rect) (rect_height rect)
              else return str
    printStringXMF dpy pixmap (ddEngineState dd) gc (sTextColor style) (sTextBgColor style) x y str'

-- | Implementation of @calcWidgetPlace@ for decoration engines based on @TextDecoration@.
calcTextWidgetPlace :: (TextWidget widget,
                        DecorationEngineState engine ~ XMonadFont,
                        DecorationEngine engine widget Window)
                    => engine widget Window
                    -> DrawData engine widget
                    -> widget
                    -> X WidgetPlace
calcTextWidgetPlace deco dd widget = do
    str <- widgetString dd widget
    let w = rect_width (ddDecoRect dd)
        h = rect_height (ddDecoRect dd)
        font = ddEngineState dd
    withDisplay $ \dpy -> do
      width <- fi <$> textWidthXMF dpy (ddEngineState dd) str
      (a, d) <- textExtentsXMF font str
      let height = a + d
          y = fi $ (h - fi height) `div` 2
          y0 = y + fi a
          rect = Rectangle 0 y width (fi height)
      return $ WidgetPlace y0 rect

-- | Add decoration to existing layout. Widgets are indicated by text fragments, like @"[+]"@.
-- Geometry is simple: a horizontal panel at the top of each window, going for the full width
-- of the window.
textDecoration :: (Shrinker shrinker)
               => shrinker                -- ^ String shrinker, for example @shrinkText@
               -> Theme TextDecoration StandardWidget  -- ^ Decoration theme (font, colors, widgets, etc)
               -> l Window                -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget DefaultGeometry shrinker) l Window
textDecoration shrinker theme = decorationEx shrinker theme TextDecoration def

