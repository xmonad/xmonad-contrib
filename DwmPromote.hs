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
-- Dwm-like promote function for xmonad.
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
import StackSet hiding (promote)
import qualified Data.Map as M

dwmpromote :: X ()
dwmpromote = windows promote

promote :: (Integral i, Ord a) => StackSet i j a -> StackSet i j a
promote w = maybe w id $ do
    a <- peek w -- fail if null
    let stack = index (current w) w
        newstack = swap a (next stack a) stack
    return $ w { stacks = M.adjust (\(f,_) -> (f, newstack)) (current w) (stacks w),
                 focus = M.insert (current w) (head newstack) (focus w) }
  where
    next s a | head s /= a = head s -- focused is not master
             | length s > 1 = s !! 1
             | otherwise = a
