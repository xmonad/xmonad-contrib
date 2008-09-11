{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Kde
-- Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
--
-- This module provides a config suitable for use with the KDE desktop
-- environment.

module XMonad.Config.Kde (
    -- * Usage
    -- $usage
    kdeConfig
    ) where

import XMonad
import XMonad.Config.Desktop

import qualified Data.Map as M

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Kde
-- >
-- > main = xmonad kdeConfig
-- 

kdeConfig = desktopConfig
    { terminal = "konsole"
    , keys     = \c -> kdeKeys c `M.union` keys desktopConfig c }

kdeKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "dcop kdesktop default popupExecuteCommand")
    , ((modm .|. shiftMask, xK_q), spawn "dcop kdesktop default logout")
    ]
