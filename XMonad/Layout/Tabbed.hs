{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Tabbed
-- Description :  A tabbed layout.
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
    , simpleTabbedLeft, tabbedLeft, addTabsLeft
    , simpleTabbedRight, tabbedRight, addTabsRight
    , simpleTabbedBottomAlways, tabbedBottomAlways, addTabsBottomAlways
    , simpleTabbedLeftAlways, tabbedLeftAlways, addTabsLeftAlways
    , simpleTabbedRightAlways, tabbedRightAlways, addTabsRightAlways
    , Theme (..)
    , def
    , TabbedDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    , TabbarShown, Direction2D(..)
    ) where

import XMonad.Prelude

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest ( Simplest(Simplest) )
import XMonad.Util.Types (Direction2D(..))

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
-- > myLayout = tabbed shrinkText def ||| Full ||| etc..
--
-- and then:
--
-- > main = xmonad def { layoutHook = myLayout }
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
-- > myTabConfig = def { inactiveBorderColor = "#FF0000"
-- >                   , activeTextColor = "#00FF00"}
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
-- > main = xmonad def { layoutHook = simpleTabbed }
simpleTabbed :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbed = tabbed shrinkText def

simpleTabbedAlways :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedAlways = tabbedAlways shrinkText def

-- | A bottom-tabbed layout with the default xmonad Theme.
simpleTabbedBottom :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedBottom = tabbedBottom shrinkText def

-- | A bottom-tabbed layout with the default xmonad Theme.
simpleTabbedBottomAlways :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
simpleTabbedBottomAlways = tabbedBottomAlways shrinkText def

-- | A side-tabbed layout with the default xmonad Theme.
simpleTabbedLeft, simpleTabbedRight :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker)
                                        Simplest Window
simpleTabbedLeft = tabbedLeft shrinkText def
simpleTabbedRight = tabbedRight shrinkText def

-- | A side-tabbed layout with the default xmonad Theme.
simpleTabbedLeftAlways, simpleTabbedRightAlways :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker)
                                                  Simplest Window
simpleTabbedLeftAlways = tabbedLeftAlways shrinkText def
simpleTabbedRightAlways = tabbedRightAlways shrinkText def

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

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and theme.
tabbedLeft, tabbedRight :: (Eq a, Shrinker s) => s -> Theme
                        -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedLeft s c = addTabsLeft s c Simplest
tabbedRight s c = addTabsRight s c Simplest

-- | A layout decorated with tabs and the possibility to set a custom
-- shrinker and theme.
tabbedLeftAlways, tabbedRightAlways :: (Eq a, Shrinker s) => s -> Theme
                                    -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbedLeftAlways s c = addTabsLeftAlways s c Simplest
tabbedRightAlways s c = addTabsRightAlways s c Simplest

-- Layout Modifiers

-- | A layout modifier that uses the provided shrinker and theme to add tabs to any layout.
addTabs :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
        -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabs = createTabs WhenPlural U

addTabsAlways :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
              -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsAlways = createTabs Always U

-- | A layout modifier that uses the provided shrinker and theme to add tabs to the bottom of any layout.
addTabsBottom :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
              -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsBottom = createTabs WhenPlural D

addTabsBottomAlways :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
                    -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsBottomAlways = createTabs Always D

-- | A layout modifier that uses the provided shrinker and theme to add tabs to the side of any layout.
addTabsRight, addTabsLeft :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
                            -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsRight = createTabs WhenPlural R
addTabsLeft = createTabs WhenPlural L

addTabsRightAlways, addTabsLeftAlways :: (Eq a, LayoutClass l a, Shrinker s) => s -> Theme -> l a
                                      -> ModifiedLayout (Decoration TabbedDecoration s) l a
addTabsRightAlways = createTabs Always R
addTabsLeftAlways = createTabs Always L

-- Tab creation abstractions.  Internal use only.

-- Create tabbar when required at the given location with the given
-- shrinker and theme to the supplied layout.
createTabs                ::(Eq a, LayoutClass l a, Shrinker s) => TabbarShown -> Direction2D -> s
                          -> Theme -> l a -> ModifiedLayout (Decoration TabbedDecoration s) l a

createTabs sh loc tx th = decoration tx th (Tabbed loc sh)

data TabbarShown = Always | WhenPlural deriving (Read, Show, Eq)

data TabbedDecoration a = Tabbed Direction2D TabbarShown deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco (Tabbed U _ ) = "Tabbed"
    describeDeco (Tabbed D _ ) = "Tabbed Bottom"
    describeDeco (Tabbed L _ ) = "Tabbed Left"
    describeDeco (Tabbed R _ ) = "Tabbed Right"
    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <- findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration (Tabbed lc sh) wt ht _ s wrs (w,r@(Rectangle x y wh hh))
        = if (sh == Always && numWindows > 0) || numWindows > 1
          then Just $ case lc of
                        U -> upperTab
                        D -> lowerTab
                        L -> leftTab
                        R -> rightTab
          else Nothing
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (S.integrate s)
              loc k h i = k + fi ((h * fi i) `div` max 1 (fi $ length ws))
              esize k h = fi $ maybe k (\i -> loc k h (i+1) - loc k h i) $ w `elemIndex` ws
              wid = esize x wh
              n k h = maybe k (loc k h) $ w `elemIndex` ws
              nx = n x wh
              upperTab = Rectangle nx  y wid (fi ht)
              lowerTab = Rectangle nx (y + fi (hh - ht)) wid (fi ht)
              fixHeightLoc i = y + fi ht * fi i
              fixHeightTab k = Rectangle k
                (maybe y fixHeightLoc
                 $ w `elemIndex` ws) (fi wt) (fi ht)
              rightTab = fixHeightTab (x + fi (wh - wt))
              leftTab = fixHeightTab x
              numWindows = length ws
    shrink (Tabbed loc _ ) (Rectangle _ _ dw dh) (Rectangle x y w h)
        = case loc of
            U -> Rectangle x (y + fi dh) w (h - dh)
            D -> Rectangle x y w (h - dh)
            L -> Rectangle (x + fi dw) y (w - dw) h
            R -> Rectangle x y (w - dw) h
