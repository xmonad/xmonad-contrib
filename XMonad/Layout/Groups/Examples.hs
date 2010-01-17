{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups.Examples
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Quentin Moser <moserq@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utility functions and example layouts for "XMonad.Layout.Groups".
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups.Examples ( -- * Usage
                                       -- $usage

                                       -- * Example: Wmii-like layout
                                       -- $example1
                                       wmiiLike
                                     , zoomGroupIn
                                     , zoomGroupOut
                                     , zoomGroupReset
                                     , toggleGroupFull
                                     , groupToNextLayout
                                     , groupToFullLayout
                                     , groupToTabbedLayout
                                     , groupToVerticalLayout

                                       -- * Example: Row of columns
                                       -- $example2
                                     , rowOfColumns
                                     , zoomColumnIn
                                     , zoomColumnOut
                                     , zoomColumnReset
                                     , toggleColumnFull
                                     , zoomWindowIn
                                     , zoomWindowOut
                                     , zoomWindowReset
                                     , toggleWindowFull

                                       -- * Example: Tiled tab groups
                                       -- $example3
                                     , tallTabs
                                     , mirrorTallTabs
                                     , fullTabs
                                     , TiledTabsConfig(..)
                                     , defaultTiledTabsConfig
                                     , increaseNMasterGroups
                                     , decreaseNMasterGroups
                                     , shrinkMasterGroups
                                     , expandMasterGroups
                                     , nextOuterLayout

                                       -- * Useful actions
                                       -- $actions

                                       -- ** Layout-generic actions
                                     , swapUp
                                     , swapDown
                                     , swapMaster
                                     , focusUp
                                     , focusDown
                                     , focusMaster
                                     , toggleFocusFloat

                                       -- ** 'G.Groups'-secific actions
                                     , swapGroupUp
                                     , swapGroupDown
                                     , swapGroupMaster
                                     , focusGroupUp
                                     , focusGroupDown
                                     , focusGroupMaster
                                     , moveToGroupUp
                                     , moveToGroupDown
                                     , moveToNewGroupUp
                                     , moveToNewGroupDown
                                     , splitGroup

                                       -- * Other useful stuff, re-exports
                                     , GroupEQ
                                     , shrinkText
                                     , defaultTheme
                                     ) where

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import qualified XMonad.Layout.Groups as G

import XMonad.Layout.ZoomRow
import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MessageControl
import XMonad.Layout.Decoration

import XMonad.Actions.MessageFeedback

import Control.Monad (unless)
import qualified Data.Map as M

-- $usage
-- This module contains example 'G.Groups'-based layouts, and
-- 'X' actions that are useful when using them. You can either
-- import this module directly, or look at its source
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
--   manner. For this, you should take a look at the \"Useful Actions\"
--   section.
--
-- This module exports many operations with the same names as
--   'G.ModifySpec's from "XMonad.Layout.Groups", so if you want
--   to import both, we suggest to import "XMonad.Layout.Groups"
--   qualified:
--
-- > import qualified XMonad.Layout.Groups as G
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


-- * Example 1: Wmii-like layout

-- $example1
-- A layout inspired by the one used by the wmii (<http://wmii.suckless.org>).
-- Windows groups are arranged in a horizontal row, and each group can lay out 
-- its windows
--
--   * by maximizing the focused one
--
--   * by tabbing them (wmii uses a stacked layout, but I'm too lazy to write it)
--
--   * by arranging them in a column.
--
-- As the groups are arranged in a 'ZoomRow', the width of each group can be increased
-- or decreased at will. Groups can also be set to use the whole screen whenever they 
-- have focus.
--
-- To use this layout, add 'wmiiLike' (with a 'Shrinker' and decoration 'Theme' as 
-- parameters) to your layout hook, for example:
--
-- > myLayout = wmiiLike shrinkText defaultTheme
--
-- To be able to zoom in and out of groups, change their inner layout, etc.,
-- create key bindings for the relevant actions:
--
-- > ((modMask, xK_f), toggleGroupFull)
--
-- and so on.

wmiiLike s t = G.group innerLayout zoomRowG
    where column = named "Column" $ Tall 0 (3/100) (1/2)
          tabs = named "Tabs" $ tabbed s t
          innerLayout = renamed [CutWordsLeft 2] $ ignore NextLayout 
                        $ ignore (JumpToLayout "") $ unEscape 
                           $ column ||| tabs ||| Full

-- | Increase the width of the focused group
zoomGroupIn :: X ()
zoomGroupIn = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomIn

-- | Decrease the size of the focused group
zoomGroupOut :: X ()
zoomGroupOut = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomOut

-- | Reset the size of the focused group to the default
zoomGroupReset :: X ()
zoomGroupReset = sendMessage $ G.ToEnclosing $ SomeMessage $ zoomReset

-- | Toggle whether the currently focused group should be maximized
-- whenever it has focus.
toggleGroupFull :: X ()
toggleGroupFull = sendMessage $ G.ToEnclosing $ SomeMessage $ ZoomFullToggle

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


-- * Example 2: Row of columns

-- $example2
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
zoomColumnIn = zoomGroupIn

-- | Decrease the width of the focused column
zoomColumnOut :: X ()
zoomColumnOut = zoomGroupOut

-- | Reset the width of the focused column
zoomColumnReset :: X ()
zoomColumnReset = zoomGroupReset

-- | Toggle whether the currently focused column should
-- take up all available space whenever it has focus
toggleColumnFull :: X ()
toggleColumnFull = toggleGroupFull

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


-- * Example 3: Tabbed groups in a Tall/Full layout.

-- $example3
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
-- for the tabs. If you're happy with defaults, you can use 'defaultTiledTabsConfig':
--
-- > myLayout = tallTabs defaultTiledTabsConfig
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

defaultTiledTabsConfig :: TiledTabsConfig DefaultShrinker
defaultTiledTabsConfig = TTC 1 0.5 (3/100) 1 0.5 (3/100) shrinkText defaultTheme

fullTabs c = G.group (_tabs c) $ Full ||| _vert c ||| _horiz c 

tallTabs c = G.group (_tabs c) $ _vert c ||| _horiz c ||| Full

mirrorTallTabs c = G.group (_tabs c) $ _horiz c ||| Full ||| _vert c

_tabs c = named "Tabs" $ tabbed (tabsShrinker c) (tabsTheme c)

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
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage $ Expand

-- | Rotate the available outer layout algorithms
nextOuterLayout :: X ()
nextOuterLayout = sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout


-- * Useful actions

-- $actions
-- "XMonad.Layout.Groups"-based layouts do not have the same notion
-- of window ordering as the rest of XMonad. For this reason, the usual
-- ways of reordering windows and moving focus do not work with them.
-- "XMonad.Layout.Groups" provides 'Message's that can be used to obtain
-- the right effect.
--
-- But what if you want to use both 'G.Groups' and other layouts?
-- This module provides actions that try to send 'G.GroupsMessage's, and
-- fall back to the classic way if the current layout doesn't hande them.
-- They are in the section called \"Layout-generic actions\".
-- 
-- The sections \"Groups-specific actions\" contains actions that don't make
-- sense for non-'G.Groups'-based layouts. These are simply wrappers around
-- the equivalent 'G.GroupsMessage's, but are included so you don't have to
-- write @sendMessage $ Modify $ ...@ everytime.

-- ** Layout-generic actions
-- #Layout-generic actions#

alt :: G.ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (G.Modify f) $ windows g

alt2 :: G.GroupsMessage -> X () -> X ()
alt2 m x = do b <- send m
              unless b x

-- | Swap the focused window with the previous one
swapUp :: X ()
swapUp = alt G.swapUp W.swapUp

-- | Swap the focused window with the next one
swapDown :: X ()
swapDown = alt G.swapDown W.swapDown

-- | Swap the focused window with the master window
swapMaster :: X ()
swapMaster = alt G.swapMaster W.swapMaster

-- | If the focused window is floating, focus the next floating
-- window. otherwise, focus the next non-floating one.
focusUp :: X ()
focusUp = ifFloat focusFloatUp focusNonFloatUp

-- | If the focused window is floating, focus the next floating
-- window. otherwise, focus the next non-floating one.
focusDown :: X ()
focusDown = ifFloat focusFloatDown focusNonFloatDown

-- | Move focus to the master window
focusMaster :: X ()
focusMaster = alt G.focusMaster W.shiftMaster

-- | Move focus between the floating and non-floating layers
toggleFocusFloat :: X ()
toggleFocusFloat = ifFloat focusNonFloat focusFloatUp

-- *** Floating layer helpers

getFloats :: X [Window]
getFloats = gets $ M.keys . W.floating . windowset

getWindows :: X [Window]
getWindows = gets $ W.integrate' . W.stack . W.workspace . W.current . windowset

ifFloat :: X () -> X () -> X ()
ifFloat x1 x2 = withFocused $ \w -> do floats <- getFloats
                                       if elem w floats then x1 else x2

focusNonFloat :: X ()
focusNonFloat = alt2 G.Refocus helper
    where helper = withFocused $ \w -> do 
                     ws <- getWindows
                     floats <- getFloats
                     let (before,  after) = span (/=w) ws
                     case filter (flip notElem floats) $ after ++ before of
                       [] -> return ()
                       w':_ -> focus w'

focusHelper :: (Bool -> Bool) -- ^ if you want to focus a floating window, 'id'.
                              -- if you want a non-floating one, 'not'.
            -> ([Window] -> [Window]) -- ^ if you want the next window, 'id'.
                                      -- if you want the previous one, 'reverse'.
            -> X ()
focusHelper f g = withFocused $ \w -> do
                 ws <- getWindows
                 let (before, _:after) = span (/=w) ws
                 let toFocus = g $ after ++ before
                 floats <- getFloats
                 case filter (f . flip elem floats) toFocus of
                   [] -> return ()
                   w':_ -> focus w'


focusNonFloatUp :: X ()
focusNonFloatUp = alt2 (G.Modify G.focusUp) $ focusHelper not reverse

focusNonFloatDown :: X ()
focusNonFloatDown = alt2 (G.Modify G.focusDown) $ focusHelper not id

focusFloatUp :: X ()
focusFloatUp = focusHelper id reverse
                 
focusFloatDown :: X ()
focusFloatDown = focusHelper id id


-- ** Groups-specific actions

wrap :: G.ModifySpec -> X ()
wrap = sendMessage . G.Modify

-- | Swap the focused group with the previous one
swapGroupUp :: X ()
swapGroupUp = wrap G.swapGroupUp

-- | Swap the focused group with the next one
swapGroupDown :: X ()
swapGroupDown = wrap G.swapGroupDown

-- | Swap the focused group with the master group
swapGroupMaster :: X ()
swapGroupMaster = wrap G.swapGroupMaster

-- | Move the focus to the previous group
focusGroupUp :: X ()
focusGroupUp = wrap G.focusGroupUp

-- | Move the focus to the next group
focusGroupDown :: X ()
focusGroupDown = wrap G.focusGroupDown

-- | Move the focus to the master group
focusGroupMaster :: X ()
focusGroupMaster = wrap G.focusGroupMaster

-- | Move the focused window to the previous group. The 'Bool' argument
-- determines what will be done if the focused window is in the very first
-- group: Wrap back to the end ('True'), or create a new group before
-- it ('False').
moveToGroupUp :: Bool -> X ()
moveToGroupUp = wrap . G.moveToGroupUp

-- | Move the focused window to the next group. The 'Bool' argument
-- determines what will be done if the focused window is in the very last
-- group: Wrap back to the beginning ('True'), or create a new group after
-- it ('False').
moveToGroupDown :: Bool -> X ()
moveToGroupDown = wrap . G.moveToGroupDown

-- | Move the focused window to a new group before the current one
moveToNewGroupUp :: X ()
moveToNewGroupUp = wrap G.moveToNewGroupUp

-- | Move the focused window to a new group after the current one
moveToNewGroupDown :: X ()
moveToNewGroupDown = wrap G.moveToNewGroupDown

-- | Split the focused group in two at the position of the focused
-- window.
splitGroup :: X ()
splitGroup = wrap G.splitGroup
