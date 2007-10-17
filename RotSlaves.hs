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
	-- $usag
	rotSlaves', rotSlavesUp, rotSlavesDown,
	rotAll', rotAllUp, rotAllDown
	) where

import StackSet
import Operations
import XMonad

-- $usage
--
-- To use this module, import it with:
--
-- > import XMonadContrib.RotSlaves
--
-- and add a keybinding:
--
-- > , ((modMask .|. shiftMask, xK_Tab   ), rotSlavesUp)
--
--
-- This operation will rotate all windows except the master window, while the focus
-- stays where it is. It is useful together with the TwoPane-Layout (see XMonadContrib.TwoPane).

-- %import XMonadContrib.RotSlaves
-- %keybind , ((modMask .|. shiftMask, xK_Tab   ), rotSlavesUp)

-- | Rotate the windows in the current stack excluding the first one
rotSlavesUp,rotSlavesDown :: X ()
rotSlavesUp   = windows $ modify' (rotSlaves' (\l -> (tail l)++[head l]))
rotSlavesDown = windows $ modify' (rotSlaves' (\l -> [last l]++(init l)))

rotSlaves' :: ([a] -> [a]) -> Stack a -> Stack a
rotSlaves' _ s@(Stack _ [] []) = s
rotSlaves' f   (Stack t [] rs) = Stack t [] (f rs)                -- Master has focus
rotSlaves' f s@(Stack _ ls _ ) = Stack t' (reverse revls') rs'    -- otherwise
    where  (master:ws)     = integrate s
           (revls',t':rs') = splitAt (length ls) (master:(f ws))

-- | Rotate the windows in the current stack
rotAllUp,rotAllDown :: X ()
rotAllUp   = windows $ modify' (rotAll' (\l -> (tail l)++[head l]))
rotAllDown = windows $ modify' (rotAll' (\l -> [last l]++(init l)))

rotAll' :: ([a] -> [a]) -> Stack a -> Stack a
rotAll' f s = Stack r (reverse revls) rs
    where (revls,r:rs) = splitAt (length (up s)) (f (integrate s))
