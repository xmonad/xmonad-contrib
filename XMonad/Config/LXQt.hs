{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.LXQt
-- Copyright    : (c) Petr Shevtsov <petr.shevtsov@gmail.com>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides a config suitable for use with the LXQt desktop
-- environment.

module XMonad.Config.LXQt (
    -- * Usage
    -- $usage
    lxqtConfig,
    desktopLayoutModifiers
    ) where

import XMonad
import XMonad.Config.Desktop

import qualified Data.Map as M

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.LXQt
-- >
-- > main = xmonad lxqtConfig
--
-- For example of how to further customize @lxqtConfig@ see "XMonad.Config.Desktop".

lxqtConfig = desktopConfig
    { terminal = "qterminal"
    , keys     = lxqtKeys <+> keys desktopConfig }

lxqtKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "lxqt-runner")
    , ((modm .|. shiftMask, xK_q), spawn "lxqt-leave")
    ]
