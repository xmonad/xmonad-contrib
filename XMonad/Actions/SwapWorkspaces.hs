-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.SwapWorkspaces
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
                                     swapWorkspaces
                                    ) where

import XMonad.StackSet

-- $usage
-- Add this import to your Config.hs:
--
-- > import XMonad.Actions.SwapWorkspaces
--
-- Throw this in your keys definition:
--
-- > ++
-- > [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
-- >     | (i, k) <- zip workspaces [xK_1 ..]]

-- %import XMonad.Actions.SwapWorkspaces
-- %keybindlist ++
-- %keybindlist [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
-- %keybindlist     | (i, k) <- zip workspaces [xK_1 ..]]
--
-- After installing this update, if you're on workspace 1, hitting mod-ctrl-5
-- will swap workspaces 1 and 5.

-- | Swaps the currently focused workspace with the given workspace tag, via
--   @swapWorkspaces@.
swapWithCurrent :: Eq i => i -> StackSet i l a s sd -> StackSet i l a s sd
swapWithCurrent t s = swapWorkspaces t (tag $ workspace $ current s) s

-- | Takes two workspace tags and an existing XMonad.StackSet and returns a new
--   one with the two corresponding workspaces' tags swapped.
swapWorkspaces :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
swapWorkspaces t1 t2 = mapWorkspace swap
    where swap w = if      tag w == t1 then w { tag = t2 }
                   else if tag w == t2 then w { tag = t1 }
                   else w
