-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.SwapWorkspaces
-- Description :  Swap workspace tags without having to move individual windows.
-- Copyright   :  (c) Devin Mullins <me@twifkak.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Lets you swap workspace tags, so you can keep related ones next to
-- each other, without having to move individual windows.
--
-----------------------------------------------------------------------------

module XMonad.Actions.SwapWorkspaces (
                                     -- * Usage
                                     -- $usage
                                     swapWithCurrent,
                                     swapTo,
                                     swapWorkspaces,
                                     Direction1D(..)
                                    ) where

import XMonad (windows, X())
import XMonad.StackSet
import XMonad.Actions.CycleWS
import XMonad.Util.WorkspaceCompare


-- $usage
-- Add this import to your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.SwapWorkspaces
--
-- Then throw something like this in your keys definition:
--
-- > ++
-- > [((modm .|. controlMask, k), windows $ swapWithCurrent i)
-- >     | (i, k) <- zip workspaces [xK_1 ..]]
--
-- After installing this update, if you're on workspace 1, hitting mod-ctrl-5
-- will swap workspaces 1 and 5.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Swaps the currently focused workspace with the given workspace tag, via
--   @swapWorkspaces@.
swapWithCurrent :: Eq i => i -> StackSet i l a s sd -> StackSet i l a s sd
swapWithCurrent t s = swapWorkspaces t (currentTag s) s

-- | Say @swapTo Next@ or @swapTo Prev@ to move your current workspace.
-- This is an @X ()@ so can be hooked up to your keybindings directly.
swapTo :: Direction1D -> X ()
swapTo dir = findWorkspace getSortByIndex dir anyWS 1 >>= windows . swapWithCurrent

-- | Takes two workspace tags and an existing XMonad.StackSet and returns a new
--   one with the two corresponding workspaces' tags swapped.
swapWorkspaces :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
swapWorkspaces t1 t2 = mapWorkspace swap
    where swap w
            | tag w == t1 = w { tag = t2 }
            | tag w == t2 = w { tag = t1 }
            | otherwise = w
