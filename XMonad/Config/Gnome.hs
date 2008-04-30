{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Gnome
-- Copyright    : (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <sjanssen@cse.unl.edu>
--
-- This module provides a config suitable for use with the GNOME desktop
-- environment.

module XMonad.Config.Gnome (
    -- * Usage
    -- -- $usage
    gnomeConfig
    ) where

import XMonad
import XMonad.Config.Desktop

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Gnome
-- >
-- > main = xmonad gnomeConfig
-- 

gnomeConfig = desktopConfig { terminal = "gnome-terminal" }
