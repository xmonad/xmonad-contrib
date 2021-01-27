{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Xfce
-- Copyright    : (c) Ivan Miljenovic <Ivan.Miljenovic@gmail.com>
-- License      : BSD
--
-- Maintainer   :  none
-- Stability    :  unstable
-- Portability  :  unportable
--
-- This module provides a config suitable for use with the Xfce desktop
-- environment.

module XMonad.Config.Xfce (
    -- * Usage
    -- $usage
    xfceConfig,
    desktopLayoutModifiers
    ) where

import XMonad
import XMonad.Config.Desktop

import qualified Data.Map as M

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Xfce
-- >
-- > main = xmonad xfceConfig
--
-- For examples of how to further customize @xfceConfig@ see "XMonad.Config.Desktop".

xfceConfig = desktopConfig
    { terminal = "xfce4-terminal"
    , keys     = xfceKeys <+> keys desktopConfig }

xfceKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "xfrun4")
    , ((modm .|. shiftMask, xK_p), spawn "xfce4-appfinder")
    , ((modm .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    ]
