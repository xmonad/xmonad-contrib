{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups.Wmii
-- Description :  A wmii-like layout algorithm.
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  stable
-- Portability :  unportable
--
-- A wmii-like layout algorithm.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups.Wmii ( -- * Usage
                                   -- $usage

                                   wmii
                                 , zoomGroupIn
                                 , zoomGroupOut
                                 , zoomGroupReset
                                 , toggleGroupFull
                                 , groupToNextLayout
                                 , groupToFullLayout
                                 , groupToTabbedLayout
                                 , groupToVerticalLayout

                                   -- * Useful re-exports
                                 , shrinkText
                                 , def
                                 , module XMonad.Layout.Groups.Helpers ) where

import XMonad

import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Examples
import XMonad.Layout.Groups.Helpers

import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.MessageControl
import XMonad.Layout.Simplest


-- $usage
-- This module provides a layout inspired by the one used by the wmii
-- (<http://wmii.suckless.org>) window manager.
-- Windows are arranged into groups in a horizontal row, and each group can lay out
-- its windows
--
--   * by maximizing the focused one
--
--   * by tabbing them (wmii uses a stacked layout, but I'm too lazy to write it)
--
--   * by arranging them in a column.
--
-- As the groups are arranged in a 'ZoomRow', the relative width of each group can be
-- increased or decreased at will. Groups can also be set to use the whole screen
-- whenever they have focus.
--
-- You can use the contents of this module by adding
--
-- > import XMonad.Layout.Groups.Wmii
--
-- to the top of your @.\/.xmonad\/xmonad.hs@, and adding 'wmii'
-- (with a 'Shrinker' and decoration 'Theme' as
-- parameters) to your layout hook, for example:
--
-- > myLayout = wmii shrinkText def
--
-- To be able to zoom in and out of groups, change their inner layout, etc.,
-- create key bindings for the relevant actions:
--
-- > ((modMask, xK_f), toggleGroupFull)
--
-- and so on.
--
-- For more information on how to extend your layout hook and key bindings, see
-- "XMonad.Doc.Extending".
--
-- Finally, you will probably want to be able to move focus and windows
-- between groups in a consistent fashion. For this, you should take a look
-- at the "XMonad.Layout.Groups.Helpers" module, whose contents are re-exported
-- by this module.

-- | A layout inspired by wmii
wmii s t = G.group innerLayout zoomRowG
    where column = named "Column" $ Tall 0 (3/100) (1/2)
          tabs = named "Tabs" Simplest
          innerLayout = renamed [CutWordsLeft 3]
                        $ addTabs s t
                        $ ignore NextLayout
                        $ ignore (JumpToLayout "") $ unEscape
                           $ column ||| tabs ||| Full

-- | Increase the width of the focused group
zoomGroupIn :: X ()
zoomGroupIn = zoomColumnIn

-- | Decrease the size of the focused group
zoomGroupOut :: X ()
zoomGroupOut = zoomColumnOut

-- | Reset the size of the focused group to the default
zoomGroupReset :: X ()
zoomGroupReset = zoomColumnReset

-- | Toggle whether the currently focused group should be maximized
-- whenever it has focus.
toggleGroupFull :: X ()
toggleGroupFull = toggleColumnFull

-- | Rotate the layouts in the focused group.
groupToNextLayout :: X ()
groupToNextLayout = sendMessage $ escape NextLayout

-- | Switch the focused group to the \"maximized\" layout.
groupToFullLayout :: X ()
groupToFullLayout = sendMessage $ escape $ JumpToLayout "Full"

-- | Switch the focused group to the \"tabbed\" layout.
groupToTabbedLayout :: X ()
groupToTabbedLayout = sendMessage $ escape $ JumpToLayout "Tabs"

-- | Switch the focused group to the \"column\" layout.
groupToVerticalLayout :: X ()
groupToVerticalLayout = sendMessage $ escape $ JumpToLayout "Column"
