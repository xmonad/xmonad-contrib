{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups.Examples
-- Description :  Example layouts for "XMonad.Layout.Groups".
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Example layouts for "XMonad.Layout.Groups".
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups.Examples ( -- * Usage
                                       -- $usage

                                       -- * Example: Row of columns
                                       -- $example1
                                       rowOfColumns
                                     , zoomColumnIn
                                     , zoomColumnOut
                                     , zoomColumnReset
                                     , toggleColumnFull
                                     , zoomWindowIn
                                     , zoomWindowOut
                                     , zoomWindowReset
                                     , toggleWindowFull

                                       -- * Example: Tiled tab groups
                                       -- $example2
                                     , tallTabs
                                     , mirrorTallTabs
                                     , fullTabs
                                     , TiledTabsConfig(..)
                                     , def
                                     , increaseNMasterGroups
                                     , decreaseNMasterGroups
                                     , shrinkMasterGroups
                                     , expandMasterGroups
                                     , nextOuterLayout


                                       -- * Useful re-exports and utils
                                     , module XMonad.Layout.Groups.Helpers
                                     , shrinkText
                                     , GroupEQ(..)
                                     , zoomRowG
                                     ) where

import XMonad

import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers

import XMonad.Layout.ZoomRow
import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest


-- $usage
-- This module contains example 'G.Groups'-based layouts.
-- You can either import this module directly, or look at its source
-- for ideas of how "XMonad.Layout.Groups" may be used.
--
-- You can use the contents of this module by adding
--
-- > import XMonad.Layout.Groups.Examples
--
-- to the top of your @.\/.xmonad\/xmonad.hs@.
--
-- For more information on using any of the layouts, jump directly
--   to its \"Example\" section.
--
-- Whichever layout you choose to use, you will probably want to be
--   able to move focus and windows between groups in a consistent
--   manner. For this, you should take a look at the functions from
--   the "XMonad.Layout.Groups.Helpers" module, which are all
--   re-exported by this module.
--
-- For more information on how to extend your layour hook and key bindings, see
--   "XMonad.Doc.Extending".


-- * Helper: ZoomRow of Group elements

-- | Compare two 'Group's by comparing the ids of their layouts.
data GroupEQ a = GroupEQ
  deriving (Show, Read)

instance Eq a => EQF GroupEQ (G.Group l a) where
    eq _ (G.G l1 _) (G.G l2 _) = G.sameID l1 l2

zoomRowG :: (Eq a, Show a, Read a, Show (l a), Read (l a))
            => ZoomRow GroupEQ (G.Group l a)
zoomRowG = zoomRowWith GroupEQ


-- * Example 1: Row of columns

-- $example1
-- A layout that arranges windows in a row of columns. It uses 'ZoomRow's for
-- both, allowing you to:
--
--  * Freely change the proportion of the screen width allocated to each column
--
--  * Freely change the proportion of a column's heigth allocated to each of its windows
--
--  * Set a column to occupy the whole screen space whenever it has focus
--
--  * Set a window to occupy its whole column whenever it has focus
--
-- to use this layout, add 'rowOfColumns' to your layout hook, for example:
--
-- > myLayout = rowOfColumns
--
-- To be able to change the sizes of columns and windows, you can create key bindings
-- for the relevant actions:
--
-- > ((modMask, xK_minus), zoomWindowOut)
--
-- and so on.

rowOfColumns = G.group column zoomRowG
    where column = renamed [CutWordsLeft 2, PrependWords "ZoomColumn"] $ Mirror zoomRow

-- | Increase the width of the focused column
zoomColumnIn :: X ()
zoomColumnIn = sendMessage $ G.ToEnclosing $ SomeMessage zoomIn

-- | Decrease the width of the focused column
zoomColumnOut :: X ()
zoomColumnOut = sendMessage $ G.ToEnclosing $ SomeMessage zoomOut

-- | Reset the width of the focused column
zoomColumnReset :: X ()
zoomColumnReset = sendMessage $ G.ToEnclosing $ SomeMessage zoomReset

-- | Toggle whether the currently focused column should
-- take up all available space whenever it has focus
toggleColumnFull :: X ()
toggleColumnFull = sendMessage $ G.ToEnclosing $ SomeMessage ZoomFullToggle

-- | Increase the heigth of the focused window
zoomWindowIn :: X ()
zoomWindowIn = sendMessage zoomIn

-- | Decrease the height of the focused window
zoomWindowOut :: X ()
zoomWindowOut = sendMessage zoomOut

-- | Reset the height of the focused window
zoomWindowReset :: X ()
zoomWindowReset = sendMessage zoomReset

-- | Toggle whether the currently focused window should
-- take up the whole column whenever it has focus
toggleWindowFull :: X ()
toggleWindowFull = sendMessage ZoomFullToggle


-- * Example 2: Tabbed groups in a Tall/Full layout.

-- $example2
-- A layout which arranges windows into tabbed groups, and the groups
-- themselves according to XMonad's default algorithm
-- (@'Tall' ||| 'Mirror' 'Tall' ||| 'Full'@). As their names
-- indicate, 'tallTabs' starts as 'Tall', 'mirrorTallTabs' starts
-- as 'Mirror' 'Tall' and 'fullTabs' starts as 'Full', but in any
-- case you can freely switch between the three afterwards.
--
-- You can use any of these three layouts by including it in your layout hook.
-- You will need to provide it with a 'TiledTabsConfig' containing the size
-- parameters for 'Tall' and 'Mirror' 'Tall', and the shrinker and decoration theme
-- for the tabs. If you're happy with defaults, you can use 'def':
--
-- > myLayout = tallTabs def
--
-- To be able to increase\/decrease the number of master groups and shrink\/expand
-- the master area, you can create key bindings for the relevant actions:
--
-- > ((modMask, xK_h), shrinkMasterGroups)
--
-- and so on.

-- | Configuration data for the "tiled tab groups" layout
data TiledTabsConfig s = TTC { vNMaster :: Int
                             , vRatio :: Rational
                             , vIncrement :: Rational
                             , hNMaster :: Int
                             , hRatio :: Rational
                             , hIncrement :: Rational
                             , tabsShrinker :: s
                             , tabsTheme :: Theme }

instance s ~ DefaultShrinker => Default (TiledTabsConfig s) where
    def = TTC 1 0.5 (3/100) 1 0.5 (3/100) shrinkText def

fullTabs c = _tab c $ G.group _tabs $ Full ||| _vert c ||| _horiz c

tallTabs c = _tab c $ G.group _tabs $ _vert c ||| _horiz c ||| Full

mirrorTallTabs c = _tab c $ G.group _tabs $ _horiz c ||| Full ||| _vert c

_tabs = named "Tabs" Simplest

_tab c l = renamed [CutWordsLeft 1] $ addTabs (tabsShrinker c) (tabsTheme c) l

_vert c = named "Vertical" $ Tall (vNMaster c) (vIncrement c) (vRatio c)

_horiz c = named "Horizontal" $ Mirror $ Tall (hNMaster c) (hIncrement c) (hRatio c)

-- | Increase the number of master groups by one
increaseNMasterGroups :: X ()
increaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN 1

-- | Decrease the number of master groups by one
decreaseNMasterGroups :: X ()
decreaseNMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ IncMasterN (-1)

-- | Shrink the master area
shrinkMasterGroups :: X ()
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Expand

-- | Rotate the available outer layout algorithms
nextOuterLayout :: X ()
nextOuterLayout = sendMessage $ G.ToEnclosing $ SomeMessage NextLayout
