{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Kde
-- Copyright    : (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <sjanssen@cse.unl.edu>
--
-- This module provides a config suitable for use with the KDE desktop
-- environment.

module XMonad.Config.Kde (
    -- * Usage
    -- -- $usage
    kdeConfig
    ) where

import XMonad
import XMonad.Config.Desktop

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Kde
-- >
-- > main = xmonad kdeConfig
-- 

kdeConfig = desktopConfig { terminal = "konsole" }
