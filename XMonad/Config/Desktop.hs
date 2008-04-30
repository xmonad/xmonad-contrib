{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Desktop
-- Copyright    : (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <sjanssen@cse.unl.edu>
--
-- This module provides a config suitable for use with a desktop
-- environment such as KDE or GNOME.

module XMonad.Config.Desktop (
    -- * Usage
    -- -- $usage
    desktopConfig,
    desktopLayoutModifiers
    ) where

import XMonad
import XMonad.Config (defaultConfig)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

desktopConfig = defaultConfig
                { logHook    = ewmhDesktopsLogHook
                , layoutHook = desktopLayoutModifiers $ layoutHook defaultConfig
                , manageHook = manageHook defaultConfig <+> manageDocks
                }

desktopLayoutModifiers = avoidStruts . ewmhDesktopsLayout

