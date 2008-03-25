{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Tabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  droundy@darcs.net, andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module XMonad.Layout.Tabbed
    ( -- * Usage:
      -- $usage
      simpleTabbed, tabbed, addTabs
    , simpleTabbedBottom, tabbedBottom, addTabsBottom
    , Theme (..)
    , defaultTheme
    , TabbedDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest ( Simplest(Simplest) )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Tabbed
--
-- Then edit your @layoutHook@ by adding the Tabbed layout:
--
-- > myLayouts = simpleTabbed ||| Full ||| etc..
--
-- or, if you want a specific theme for you tabbed layout:
--
-- > myLayouts = tabbed shrinkText defaultTheme ||| Full ||| etc..
--
-- and then:
--
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > myTabConfig = defaultTheme { inactiveBorderColor = "#FF0000"
-- >                                   , activeTextColor = "#00FF00"}
--
-- and
--
-- > mylayout = tabbed shrinkText myTabConfig ||| Full ||| etc..

-- | A tabbed layout with the default xmonad Theme. Here's a screen
-- shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/simpleTabbed.png>
--
-- This is a minimal working configuration:
--
-- > import XMonad
-- > import XMonad.Layout.DecorationMadness
-- > main = xmonad defaultConfig { layoutHook = simpleTabbed }
simpleTabbed :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbed = decoration shrinkText defaultTheme Tabbed Simplest

-- | A bottom-tabbed layout with the default xmonad Theme.
simpleTabbedBottom :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedBottom = decoration shrinkText defaultTheme TabbedBottom Simplest

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and a custom theme.
tabbed :: (Eq a, Shrinker s) => s -> Theme
       -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbed s c = decoration s c Tabbed Simplest

-- | A layout decorated with tabs at the bottom and the possibility to set a custom
-- shrinker and a custom theme.
tabbedBottom :: (Eq a, Shrinker s) => s -> Theme
       -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedBottom s c = decoration s c TabbedBottom Simplest

addTabs :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
        -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabs s c l = decoration s c Tabbed l

addTabsBottom :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
        -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsBottom s c l = decoration s c TabbedBottom l

data TabbedDecoration a = Tabbed | TabbedBottom deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco Tabbed = "Tabbed"
    describeDeco TabbedBottom = "Tabbed Bottom"
    decorationMouseDragHook _ _ _ = return ()
    pureDecoration ds _ ht _ s wrs (w,r@(Rectangle x y wh hh)) =
            if length ws <= 1
                then Nothing
                else Just $ case ds of
                    Tabbed -> Rectangle nx y wid (fi ht)
                    TabbedBottom -> Rectangle nx (y+fi(hh-ht)) wid (fi ht)
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
              loc i = (wh * fi i) `div` max 1 (fi $ length ws)
              wid = maybe (fi x) (\i -> loc (i+1) - loc i) $ w `elemIndex` ws
              nx  = maybe x (fi . loc) $ w `elemIndex` ws
    shrink ds (Rectangle _ _ _ dh) (Rectangle x y w h) = case ds of
        Tabbed -> Rectangle x (y + fi dh) w (h - dh)
        TabbedBottom -> Rectangle x y w (h - dh)
