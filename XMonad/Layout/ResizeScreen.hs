{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ResizeScreen
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout transformer to have a layout respect a given screen
-- geometry
-----------------------------------------------------------------------------

module XMonad.Layout.ResizeScreen
    ( -- * Usage:
      -- $usage
      resizeHorizontal
    , resizeVertical
    , withNewRectangle
    , ResizeScreen (..)
    ) where

import XMonad
import XMonad.Util.XUtils (fi)
import XMonad.Layout.LayoutModifier

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
resizeHorizontal i = ModifiedLayout (ResizeScreen H i)

resizeVertical :: Int -> l a -> ModifiedLayout ResizeScreen l a
resizeVertical i = ModifiedLayout (ResizeScreen V i)

withNewRectangle  :: Rectangle -> l a -> ModifiedLayout ResizeScreen l a
withNewRectangle r = ModifiedLayout (WithNewScreen r)

data ResizeScreen a = ResizeScreen ResizeMode Int
                    | WithNewScreen Rectangle
                      deriving (Read, Show)
data ResizeMode = H | V deriving (Read, Show)

instance LayoutModifier ResizeScreen a where
    modifyLayout m l re@(Rectangle x y w h) s
        | ResizeScreen H i <- m = resize (Rectangle (x + fi i) y (w - fi i) h)
        | ResizeScreen V i <- m = resize (Rectangle x (y + fi i) w (h - fi i))
        | WithNewScreen  r <- m = resize r
        | otherwise             = resize re
       where resize nr = doLayout l nr s
