{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Bepo
-- Description  : Fix keybindings for the BEPO keyboard layout.
-- Copyright    : (c) Yorick Laupa <yo.eight@gmail.com>
-- License      : BSD
--
-- Maintainer   : Yorick Laupa <yo.eight@gmail.com>
-- Stability    : stable
-- Portability  : unportable
--
-- This module fixes some of the keybindings for the francophone among you who
-- use a BEPO keyboard layout. Based on XMonad.Config.Azerty

module XMonad.Config.Bepo (
    -- * Usage
    -- $usage
    bepoConfig, bepoKeys
    ) where

import XMonad
import qualified XMonad.StackSet as W

import qualified Data.Map as M

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Bepo
-- >
-- > main = xmonad bepoConfig
--
-- If you prefer, an bepoKeys function is provided which you can use as so:
--
-- > import qualified Data.Map as M
-- > main = xmonad someConfig { keys = \c -> bepoKeys c `M.union` keys someConfig c }

bepoConfig = def { keys = bepoKeys <+> keys def }

bepoKeys conf@XConfig { modMask = modm } = M.fromList $
    ((modm, xK_semicolon), sendMessage (IncMasterN (-1)))
    : [((m .|. modm, k), windows $ f i)
          | (i, k) <- zip (workspaces conf) [0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a],
            (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
