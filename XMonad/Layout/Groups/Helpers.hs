{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types, ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups.Helpers
-- Description :  Utility functions for "XMonad.Layout.Groups".
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  stable
-- Portability :  unportable
--
-- Utility functions for "XMonad.Layout.Groups".
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups.Helpers ( -- * Usage
                                      -- $usage

                                      -- ** Layout-generic actions
                                      swapUp
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
                                    , splitGroup ) where

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import qualified XMonad.Layout.Groups as G

import XMonad.Actions.MessageFeedback (sendMessageB)

import XMonad.Prelude (unless)
import qualified Data.Map as M

-- $usage
--
-- This module provides helpers functions for use with "XMonad.Layout.Groups"-based
-- layouts. You can use its contents by adding
--
-- > import XMonad.Layout.Groups.Helpers
--
-- to the top of your @.\/.xmonad\/xmonad.hs@.
--
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
--
-- This module exports many operations with the same names as
-- 'G.ModifySpec's from "XMonad.Layout.Groups", so if you want
-- to import both, we suggest to import "XMonad.Layout.Groups"
-- qualified:
--
-- > import qualified XMonad.Layout.Groups as G
--
-- For more information on how to extend your layour hook and key bindings, see
-- "XMonad.Doc.Extending".

-- ** Layout-generic actions
-- #Layout-generic actions#

alt :: G.ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (G.Modify f) $ windows g

alt2 :: G.GroupsMessage -> X () -> X ()
alt2 m x = do b <- sendMessageB m
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
                                       if w `elem` floats then x1 else x2

focusNonFloat :: X ()
focusNonFloat = alt2 G.Refocus helper
    where helper = withFocused $ \w -> do
                     ws <- getWindows
                     floats <- getFloats
                     let (before,  after) = span (/=w) ws
                     case filter (`notElem` floats) $ after ++ before of
                       [] -> return ()
                       w':_ -> focus w'

focusHelper :: (Bool -> Bool) -- ^ if you want to focus a floating window, 'id'.
                              -- if you want a non-floating one, 'not'.
            -> ([Window] -> [Window]) -- ^ if you want the next window, 'id'.
                                      -- if you want the previous one, 'reverse'.
            -> X ()
focusHelper f g = withFocused $ \w -> do
                 ws <- getWindows
                 let (before, tail -> after) = span (/=w) ws
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
wrap x = sendMessage (G.Modify x)

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
moveToGroupUp b = wrap (G.moveToGroupUp b)

-- | Move the focused window to the next group. The 'Bool' argument
-- determines what will be done if the focused window is in the very last
-- group: Wrap back to the beginning ('True'), or create a new group after
-- it ('False').
moveToGroupDown :: Bool -> X ()
moveToGroupDown b = wrap (G.moveToGroupDown b)

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
