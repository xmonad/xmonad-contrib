{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Tabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
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
    , simpleTabbedAlways, tabbedAlways, addTabsAlways
    , simpleTabbedBottom, tabbedBottom, addTabsBottom
    , simpleTabbedBottomAlways, tabbedBottomAlways, addTabsBottomAlways
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
-- > myLayout = simpleTabbed ||| Full ||| etc..
--
-- or, if you want a specific theme for you tabbed layout:
--
-- > myLayout = tabbed shrinkText defaultTheme ||| Full ||| etc..
--
-- and then:
--
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- This layout has hardcoded behaviour for mouse clicks on tab decorations:
-- Left click on the tab switches focus to that window.
-- Middle click on the tab closes the window.
--
-- The default Tabbar behaviour is to hide it when only one window is open
-- on the workspace.  To have it always shown, use one of the layouts or
-- modifiers ending in @Always@.
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

-- Layouts

-- | A tabbed layout with the default xmonad Theme.
--
-- This is a minimal working configuration:
--
-- > import XMonad
-- > import XMonad.Layout.Tabbed
-- > main = xmonad defaultConfig { layoutHook = simpleTabbed }
simpleTabbed :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbed = tabbed shrinkText defaultTheme

simpleTabbedAlways :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedAlways = tabbedAlways shrinkText defaultTheme

-- | A bottom-tabbed layout with the default xmonad Theme.
simpleTabbedBottom :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedBottom = tabbedBottom shrinkText defaultTheme

-- | A bottom-tabbed layout with the default xmonad Theme.
simpleTabbedBottomAlways :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedBottomAlways = tabbedBottomAlways shrinkText defaultTheme

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and theme.
tabbed     :: (Eq a, Shrinker s) => s -> Theme
           -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbed s c = addTabs s c Simplest

tabbedAlways     :: (Eq a, Shrinker s) => s -> Theme
                 -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedAlways s c = addTabsAlways s c Simplest

-- | A layout decorated with tabs at the bottom and the possibility to set a custom
-- shrinker and theme.
tabbedBottom     :: (Eq a, Shrinker s) => s -> Theme
                 -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedBottom s c = addTabsBottom s c Simplest

tabbedBottomAlways     :: (Eq a, Shrinker s) => s -> Theme
                       -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedBottomAlways s c = addTabsBottomAlways s c Simplest

-- Layout Modifiers

-- | A layout modifier that uses the provided shrinker and theme to add tabs to any layout.
addTabs :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
        -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabs = createTabs WhenPlural Top

addTabsAlways :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
              -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsAlways = createTabs Always Top

-- | A layout modifier that uses the provided shrinker and theme to add tabs to the bottom of any layout.
addTabsBottom :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
              -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsBottom = createTabs WhenPlural Bottom

addTabsBottomAlways :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
                    -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsBottomAlways = createTabs Always Bottom


-- Tab creation abstractions.  Internal use only.

-- Create tabbar when required at the given location with the given
-- shrinker and theme to the supplied layout.
createTabs                ::(Eq a, LayoutClass l a, Shrinker s) => TabbarShown -> TabbarLocation -> s
                          -> Theme -> l a -> ModifiedLayout (Decoration TabbedDecoration s) l a
createTabs sh loc tx th l = decoration tx th (Tabbed loc sh) l

data TabbarLocation = Top | Bottom deriving (Read,Show)

data TabbarShown = Always | WhenPlural deriving (Read, Show, Eq)

data TabbedDecoration a = Tabbed TabbarLocation TabbarShown deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco (Tabbed Top _ ) = "Tabbed"
    describeDeco (Tabbed Bottom _ ) = "Tabbed Bottom"
    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <-findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration (Tabbed lc sh) _ ht _ s wrs (w,r@(Rectangle x y wh hh))
        = if ((sh == Always && numWindows > 0) || numWindows > 1)
          then Just $ case lc of
                        Top -> upperTab
                        Bottom -> lowerTab
          else Nothing
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
              loc i = x + fi ((wh * fi i) `div` max 1 (fi $ length ws))
              wid = fi $ maybe x (\i -> loc (i+1) - loc i) $ w `elemIndex` ws
              nx  = maybe x loc $ w `elemIndex` ws
              upperTab = Rectangle nx y wid (fi ht)
              lowerTab = Rectangle nx (y+fi(hh-ht)) wid (fi ht)
              numWindows = length ws
    shrink (Tabbed loc _ ) (Rectangle _ _ _ dh) (Rectangle x y w h)
        = case loc of
            Top -> Rectangle x (y + fi dh) w (h - dh)
            Bottom -> Rectangle x y w (h - dh)
