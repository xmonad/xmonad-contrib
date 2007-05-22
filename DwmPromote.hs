-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DwmPromote
-- Copyright   :  (c) Miikka Koskinen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  arcatan@kapsi.fi
--
-----------------------------------------------------------------------------
--
-- Dwm-like swap function for xmonad.
--
-- Swaps focused window with the master window. If focus is in the
-- master, swap it with the next window in the stack. Focus stays in the
-- master.
--
-- To use, modify your Config.hs to:
--
--      import XMonadContrib.DwmPromote
--
-- and add a keybinding or substitute promote with dwmpromote:
--
--     , ((modMask,               xK_Return), dwmpromote)
--

module XMonadContrib.DwmPromote (dwmpromote) where

import XMonad
import Operations (windows)
import StackSet

dwmpromote :: X ()
dwmpromote = windows swap

swap :: StackSet i a s -> StackSet i a s
swap = modify Empty $ \c -> case c of
    Node _ [] []     -> c
    Node t [] (x:rs) -> Node x [] (t:rs)
    Node t ls rs     -> Node t [] (ys ++ x : rs) where (x:ys) = reverse ls
