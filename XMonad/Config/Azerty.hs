{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Azerty
-- Description  : Fix some keybindings for users of French keyboard layouts.
-- Copyright    : (c) Devin Mullins <me@twifkak.com>
-- License      : BSD
--
-- Maintainer   : Devin Mullins <me@twifkak.com>
-- Stability    : stable
-- Portability  : unportable
--
-- This module fixes some of the keybindings for the francophone among you who
-- use an AZERTY keyboard layout. Config stolen from TeXitoi's config on the
-- wiki.

module XMonad.Config.Azerty (
    -- * Usage
    -- $usage
    azertyConfig, azertyKeys, belgianConfig, belgianKeys
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
-- > main = xmonad someConfig { keys = \c -> azertyKeys c <+> keys someConfig c }

azertyConfig = def { keys = azertyKeys <+> keys def }

belgianConfig = def { keys = belgianKeys <+> keys def }

azertyKeys = azertyKeysTop [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]

belgianKeys = azertyKeysTop [0x26,0xe9,0x22,0x27,0x28,0xa7,0xe8,0x21,0xe7,0xe0]

azertyKeysTop topRow conf@XConfig{modMask = modm} = M.fromList $
    [((modm, xK_semicolon), sendMessage (IncMasterN (-1)))]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) topRow,
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{z,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{z,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
