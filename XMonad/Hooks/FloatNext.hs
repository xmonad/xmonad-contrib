-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.FloatNext
-- Description :  Automatically send the next spawned window(s) to the floating layer.
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Hook and keybindings for automatically sending the next
-- spawned window(s) to the floating layer.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.FloatNext ( -- * Usage
                                -- $usage

                                -- * The hook
                                floatNextHook

                                -- * Actions
                              , floatNext
                              , toggleFloatNext
                              , floatAllNew
                              , toggleFloatAllNew

                                -- * Queries
                              , willFloatNext
                              , willFloatAllNew

                                -- * Status bar utilities
                                -- $pp
                              , willFloatNextPP
                              , willFloatAllNewPP
                              , runLogHook ) where

import XMonad
import XMonad.Hooks.ToggleHook

hookName :: String
hookName = "__float"

-- $usage
-- This module provides actions (that can be set as keybindings)
-- to automatically send the next spawned window(s) to the floating
-- layer.
--
-- You can use it by including the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.FloatNext
--
-- and adding 'floatNextHook' to your 'ManageHook':
--
-- > myManageHook = floatNextHook <+> manageHook def
--
-- The 'floatNext' and 'toggleFloatNext' functions can be used in key
-- bindings to float the next spawned window:
--
-- > , ((modm, xK_e), toggleFloatNext)
--
-- 'floatAllNew' and 'toggleFloatAllNew' are similar but float all
-- spawned windows until disabled again.
--
-- > , ((modm, xK_r), toggleFloatAllNew)

-- | This 'ManageHook' will selectively float windows as set
-- by 'floatNext' and 'floatAllNew'.
floatNextHook :: ManageHook
floatNextHook = toggleHook hookName doFloat

-- | @floatNext True@ arranges for the next spawned window to be
-- sent to the floating layer, @floatNext False@ cancels it.
floatNext :: Bool -> X ()
floatNext = hookNext hookName

toggleFloatNext :: X ()
toggleFloatNext = toggleHookNext hookName

-- | @floatAllNew True@ arranges for new windows to be
-- sent to the floating layer, @floatAllNew False@ cancels it
floatAllNew :: Bool -> X ()
floatAllNew = hookAllNew hookName

toggleFloatAllNew :: X ()
toggleFloatAllNew = toggleHookAllNew hookName

-- | Whether the next window will be set floating
willFloatNext :: X Bool
willFloatNext = willHookNext hookName

-- | Whether new windows will be set floating
willFloatAllNew :: X Bool
willFloatAllNew = willHookAllNew hookName

-- $pp
-- The following functions are used to display the current
-- state of 'floatNext' and 'floatAllNew' in your
-- "XMonad.Hooks.StatusBar.PP".
-- 'willFloatNextPP' and 'willFloatAllNewPP' should be added
-- to the 'XMonad.Hooks.StatusBar.PP.ppExtras' field of your
-- "XMonad.Hooks.StatusBar.PP".
--
-- Use 'runLogHook' to refresh the output of your 'logHook', so
-- that the effects of a 'floatNext'/... will be visible
-- immediately:
--
-- > , ((modm, xK_e), toggleFloatNext >> runLogHook)
--
-- The @String -> String@ parameters to 'willFloatNextPP' and
-- 'willFloatAllNewPP' will be applied to their output, you
-- can use them to set the text color, etc., or you can just
-- pass them 'id'.

willFloatNextPP :: (String -> String) -> X (Maybe String)
willFloatNextPP = willHookNextPP hookName

willFloatAllNewPP :: (String -> String) -> X (Maybe String)
willFloatAllNewPP = willHookAllNewPP hookName
