{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Xfce
-- Copyright    : (c) Ivan Miljenovic <Ivan.Miljenovic@gmail.com>
-- License      : BSD
--
-- Maintainer   : Ivan Miljenovic <Ivan.Miljenovic@gmail.com>
--
-- This module provides a config suitable for use with the Xfce desktop
-- environment.

module XMonad.Config.Xfce (
    -- * Usage
    -- $usage
    xfceConfig
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

xfceConfig = desktopConfig
    { terminal = "Terminal"
    , keys     = \c -> xfceKeys c `M.union` keys desktopConfig c }

xfceKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "xfrun4")
    , ((modm .|. shiftMask, xK_p), spawn "xfce4-appfinder")
    , ((modm .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    ]
