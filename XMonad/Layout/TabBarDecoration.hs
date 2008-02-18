{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TabBarDecoration
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier to add a bar of tabs to your layouts.
-----------------------------------------------------------------------------

module XMonad.Layout.TabBarDecoration
    ( -- * Usage
      -- $usage
      simpleTabBar, tabBar
    , defaultTheme, shrinkText
    , TabBarDecoration (..), XPPosition (..)
    ) where

import Data.List
import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Prompt ( XPPosition (..) )
-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TabBarDecoration
--
-- Then edit your @layoutHook@ by adding the layout you want:
--
-- > main = xmonad defaultConfig { layoutHook = simpleTabBar $ layoutHook defaultConfig}
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- 'tabBar' will give you the possibility of setting a custom shrinker
-- and a custom theme.
--
-- The deafult theme can be dynamically change with the xmonad theme
-- selector. See "XMonad.Prompt.Theme". For more themse, look at
-- "XMonad.Util.Themes"

simpleTabBar :: Eq a => l a -> ModifiedLayout (Decoration TabBarDecoration DefaultShrinker) l a
simpleTabBar = decoration shrinkText defaultTheme (TabBar Top)

tabBar :: (Eq a, Shrinker s) => s -> Theme -> XPPosition -> l a -> ModifiedLayout (Decoration TabBarDecoration s) l a
tabBar s t p = decoration s t (TabBar p)

data TabBarDecoration a = TabBar XPPosition deriving (Read, Show)

instance Eq a => DecorationStyle TabBarDecoration a where
    describeDeco  _ = "TabBar"
    decorateFirst _ = True
    shrink    _ _ r = r
    decorationMouseDragHook _ _ _ = return ()
    pureDecoration (TabBar p) _ dht (Rectangle x y wh ht) s _ (w,_) =
        if isInStack s w then Just $ Rectangle nx ny nwh (fi dht) else Nothing
        where nwh = wh `div` max 1 (fi $ length $ S.integrate s)
              ny = case p of
                     Top    -> y
                     Bottom -> y + fi ht - fi dht
              nx  = case w `elemIndex` (S.integrate s) of
                      Just i  -> x + (fi nwh * fi i)
                      Nothing -> x
