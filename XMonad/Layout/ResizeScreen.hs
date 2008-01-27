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

import Control.Arrow (second)
import Control.Applicative ((<$>))

import XMonad
import XMonad.Util.XUtils (fi)

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

resizeHorizontal :: Int -> l a -> ResizeScreen l a
resizeHorizontal = ResizeScreen H

resizeVertical :: Int -> l a -> ResizeScreen l a
resizeVertical = ResizeScreen V

withNewRectangle  :: Rectangle -> l a -> ResizeScreen l a
withNewRectangle = WithNewScreen

data ResizeScreen l a = ResizeScreen ResizeMode Int (l a)
                      | WithNewScreen Rectangle (l a)
                        deriving (Read, Show)
data ResizeMode = H | V deriving (Read, Show)

instance (LayoutClass l a) => LayoutClass (ResizeScreen l) a where
    doLayout m (Rectangle x y w h ) s
        | ResizeScreen H i l <- m = resize (ResizeScreen V i) l (Rectangle (x + fi i) y (w - fi i) h)
        | ResizeScreen V i l <- m = resize (ResizeScreen H i) l (Rectangle x (y + fi i) w (h - fi i))
        | WithNewScreen  r l <- m = resize (WithNewScreen  r) l r
        | otherwise               = return ([],Nothing)
       where resize t l' nr = second (fmap t) <$> doLayout l' nr s

    handleMessage rs m
        | ResizeScreen  t i l <- rs = go (ResizeScreen t i) l
        | WithNewScreen r   l <- rs = go (WithNewScreen  r) l
        | otherwise                 = return Nothing
        where go tp lay = do ml' <- handleMessage lay m
                             return (tp `fmap` ml')

    emptyLayout rs re
        | ResizeScreen  t i l <- rs = go (ResizeScreen t i) l
        | WithNewScreen r   l <- rs = go (WithNewScreen  r) l
        | otherwise                 = return ([],Nothing)
        where go tp lay = do (wrs,ml) <- emptyLayout lay re
                             return (wrs, tp `fmap` ml)

    description _ = []
