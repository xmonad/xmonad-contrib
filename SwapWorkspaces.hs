-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SwapWorkspaces
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
-- TODO: add quickcheck props for:
--       * double swap invariant (guarantees no 'loss' of workspaces)
--       * non-swapped ws's invariant
--
-----------------------------------------------------------------------------

module XMonadContrib.SwapWorkspaces (
                                     -- * Usage
                                     -- $usage
                                     swapWithCurrent,
                                     swapWorkspaces
                                    ) where

import StackSet

-- $usage
-- Add this import to your Config.hs:
-- > import XMonadContrib.SwapWorkspaces
--
-- Throw this in your keys definition:
-- > ++
-- > [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
-- >     | (i, k) <- zip workspaces [xK_1 ..]]
--
-- %import XMonadContrib.SwapWorkspaces
-- %keybindlist ++
-- %keybindlist [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
-- %keybindlist     | (i, k) <- zip workspaces [xK_1 ..]]

swapWithCurrent :: Eq i => i -> StackSet i l a s sd -> StackSet i l a s sd
swapWithCurrent t s = swapWorkspaces t (tag $ workspace $ current s) s

swapWorkspaces :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd
swapWorkspaces t1 t2 = mapWorkspace swap
    where swap w = if      tag w == t1 then w { tag = t2 }
                   else if tag w == t2 then w { tag = t1 }
                   else w
