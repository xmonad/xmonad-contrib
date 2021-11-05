{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ResizeScreen
-- Description :  A layout transformer to have a layout respect a given screen geometry.
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout transformer to have a layout respect a given screen
-- geometry. Mostly used with "Decoration" (the Horizontal and the
-- Vertical version will react to SetTheme and change their dimension
-- accordingly.
-----------------------------------------------------------------------------

module XMonad.Layout.ResizeScreen
    ( -- * Usage:
      -- $usage
      resizeHorizontal, resizeVertical
    , resizeHorizontalRight, resizeVerticalBottom
    , withNewRectangle
    , ResizeScreen (..)
    , ResizeMode
    ) where

import XMonad
import XMonad.Layout.Decoration

-- $usage
-- You can use this module by importing it into your
-- @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.ResizeScreen
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = resizeHorizontal 40 Full
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

resizeHorizontal :: Int -> l a -> ModifiedLayout ResizeScreen l a
resizeHorizontal i = ModifiedLayout (ResizeScreen L i)

resizeVertical :: Int -> l a -> ModifiedLayout ResizeScreen l a
resizeVertical i = ModifiedLayout (ResizeScreen T i)

resizeHorizontalRight :: Int -> l a -> ModifiedLayout ResizeScreen l a
resizeHorizontalRight i = ModifiedLayout (ResizeScreen R i)

resizeVerticalBottom :: Int -> l a -> ModifiedLayout ResizeScreen l a
resizeVerticalBottom i = ModifiedLayout (ResizeScreen B i)

withNewRectangle  :: Rectangle -> l a -> ModifiedLayout ResizeScreen l a
withNewRectangle r = ModifiedLayout (WithNewScreen r)

data ResizeScreen a = ResizeScreen ResizeMode Int
                    | WithNewScreen Rectangle
                      deriving (Read, Show)

data ResizeMode = T | B | L | R deriving (Read, Show)

instance LayoutModifier ResizeScreen a where
    modifyLayout m ws (Rectangle x y w h)
        | ResizeScreen L i <- m = resize $ Rectangle (x + fi i) y (w - fi i) h
        | ResizeScreen R i <- m = resize $ Rectangle x          y (w - fi i) h
        | ResizeScreen T i <- m = resize $ Rectangle x (y + fi i) w (h - fi i)
        | ResizeScreen B i <- m = resize $ Rectangle x  y         w (h - fi i)
        | WithNewScreen  r <- m = resize r
       where resize = runLayout ws

    pureMess (ResizeScreen d _) m
        | Just (SetTheme t) <- fromMessage m = Just $ ResizeScreen d (fi $ decoHeight t)
    pureMess _ _ = Nothing
