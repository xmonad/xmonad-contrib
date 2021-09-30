{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TabBarDecoration
-- Description :  A layout modifier to add a bar of tabs to your layouts.
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
    , def, shrinkText
    , TabBarDecoration (..), XPPosition (..)
    , module XMonad.Layout.ResizeScreen
    ) where

import XMonad.Prelude
import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.ResizeScreen
import XMonad.Prompt ( XPPosition (..) )

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TabBarDecoration
--
-- Then edit your @layoutHook@ by adding the layout you want:
--
-- > main = xmonad def { layoutHook = simpleTabBar $ layoutHook def}
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

-- | Add, on the top of the screen, a simple bar of tabs to a given
-- | layout, with the default theme and the default shrinker.
simpleTabBar :: Eq a => l a -> ModifiedLayout (Decoration TabBarDecoration DefaultShrinker)
                (ModifiedLayout ResizeScreen l) a
simpleTabBar = decoration shrinkText def (TabBar Top) . resizeVertical 20

-- | Same of 'simpleTabBar', but with the possibility of setting a
-- custom shrinker, a custom theme and the position: 'Top' or
-- 'Bottom'.
tabBar :: (Eq a, Shrinker s) => s -> Theme -> XPPosition -> l a -> ModifiedLayout (Decoration TabBarDecoration s) l a
tabBar s t p = decoration s t (TabBar p)

newtype TabBarDecoration a = TabBar XPPosition deriving (Read, Show)

instance Eq a => DecorationStyle TabBarDecoration a where
    describeDeco  _ = "TabBar"
    shrink    _ _ r = r
    decorationCatchClicksHook _ mainw _ _ = focus mainw >> return True
    pureDecoration (TabBar p) _ dht (Rectangle x y wh ht) s _ (w,_) =
        if isInStack s w then Just $ Rectangle nx ny wid (fi dht) else Nothing
        where wrs = S.integrate s
              loc i = (wh * fi i) `div` max 1 (fi $ length wrs)
              wid = maybe (fi x) (\i -> loc (i+1) - loc i) $ w `elemIndex` wrs
              ny  = case p of
                     Top    -> y
                     Bottom -> y + fi ht - fi dht
                     _      -> error "Position must be 'Top' or 'Bottom'"
              nx  = (x +) $ maybe 0 (fi . loc) $ w `elemIndex` wrs
