-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.RotSlaves
-- Copyright    : (c) Hans Philipp Annen <haphi@gmx.net>, Mischa Dieterle <der_m@freenet.de>
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Hans Philipp Annen <haphi@gmx.net>
-- Stability    : unstable
-- Portability  : unportable
--
-- Rotate all windows except the master window 
-- and keep the focus in place.
-----------------------------------------------------------------------------
module XMonadContrib.RotSlaves (
	-- $usage
	rotSlaves', rotSlaves
	) where

import qualified StackSet as SS

-- $usage
--
-- To use this module, import it with:
--
-- > import XMonadContrib.RotSlaves
--
-- and add a keybinding:
--
-- , ((modMask .|. shiftMask, xK_Tab   ), windows rotSlaves) 
--
--
-- This operation will rotate all windows except the master window, while the focus
-- stays where it is. It is usefull together with the TwoPane-Layout (see XMonadContrib.TwoPane).
--

rotSlaves :: SS.StackSet i a s sd -> SS.StackSet i a s sd
rotSlaves = SS.modify' rotSlaves'

rotSlaves' :: SS.Stack a -> SS.Stack a
rotSlaves' (SS.Stack t ls rs) | (null ls) = SS.Stack t [] ((rearRs)++(frontRs))          --Master has focus
                              | otherwise = SS.Stack t' (reverse ((master)++revls')) rs' --otherwise
    where  (frontRs, rearRs) = splitAt (max 0 ((length rs) - 1)) rs
           (ils, master)     = splitAt (max 0 ((length ls) - 1)) ls
           toBeRotated       = (reverse ils)++(t:rs)
           (revls',t':rs')   = splitAt (length ils) ((last toBeRotated):(init toBeRotated))


