{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Azerty
-- Copyright    : (c) Devin Mullins <me@twifkak.com>
-- License      : BSD
--
-- Maintainer   : Devin Mullins <me@twifkak.com>
--
-- This module fixes some of the keybindings for the francophone among you who
-- use an AZERTY keyboard layout. Config stolen from TeXitoi's config on the
-- wiki.

module XMonad.Config.Azerty (
    -- * Usage
    -- $usage
    azertyConfig, azertyKeys
    ) where

import XMonad
import qualified XMonad.StackSet as W

import qualified Data.Map as M

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Azerty
-- >
-- > main = xmonad azertyConfig
--
-- If you prefer, an azertyKeys function is provided which you can use as so:
--
-- > import qualified Data.Map as M
-- > main = xmonad someConfig { keys = \c -> azertyKeys c `M.union` keys someConfig c }

azertyConfig = defaultConfig { keys = \c -> azertyKeys c `M.union` keys defaultConfig c }

azertyKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [((modm, xK_semicolon), sendMessage (IncMasterN (-1)))]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
