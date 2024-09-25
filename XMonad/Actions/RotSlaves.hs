{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.RotSlaves
-- Description  : Rotate all windows except the master window and keep the focus in place.
-- Copyright    : (c) Hans Philipp Annen <haphi@gmx.net>, Mischa Dieterle <der_m@freenet.de>
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Hans Philipp Annen <haphi@gmx.net>
-- Stability    : stable
-- Portability  : unportable
--
-- Rotate all windows except the master window and keep the focus in
-- place.
-----------------------------------------------------------------------------
module XMonad.Actions.RotSlaves (
        -- $usage
        rotSlaves', rotSlavesUp, rotSlavesDown,
        rotAll', rotAllUp, rotAllDown,

        -- * Generic list rotations
        -- $generic
        rotUp, rotDown
        ) where

import XMonad
import XMonad.StackSet
import XMonad.Prelude

-- $usage
--
-- To use this module, import it with:
--
-- > import XMonad.Actions.RotSlaves
--
-- and add whatever keybindings you would like, for example:
--
-- > , ((modm .|. shiftMask, xK_Tab   ), rotSlavesUp)
--
-- This operation will rotate all windows except the master window,
-- while the focus stays where it is. It is useful together with the
-- TwoPane layout (see "XMonad.Layout.TwoPane").
--
-- For detailed instructions on editing your key bindings, see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.

-- | Rotate the windows in the current stack, excluding the first one
--   (master).
rotSlavesUp,rotSlavesDown :: X ()
rotSlavesUp   = windows $ modify' (rotSlaves' rotUp)
rotSlavesDown = windows $ modify' (rotSlaves' rotDown)

-- | The actual rotation, as a pure function on the window stack.
rotSlaves' :: ([a] -> [a]) -> Stack a -> Stack a
rotSlaves' _ s@(Stack _ [] []) = s
rotSlaves' f   (Stack t [] rs) = Stack t [] (f rs)                -- Master has focus
rotSlaves' f s@(Stack _ ls _ ) = Stack t' (reverse revls') rs'    -- otherwise
    where  (notEmpty -> master :| ws)      = integrate s
           (revls', notEmpty -> t' :| rs') = splitAt (length ls) (master:f ws)

-- | Rotate all the windows in the current stack.
rotAllUp,rotAllDown :: X ()
rotAllUp   = windows $ modify' (rotAll' rotUp)
rotAllDown = windows $ modify' (rotAll' rotDown)

-- | The actual rotation, as a pure function on the window stack.
rotAll' :: ([a] -> [a]) -> Stack a -> Stack a
rotAll' f s = Stack r (reverse revls) rs
    where (revls, notEmpty -> r :| rs) = splitAt (length (up s)) (f (integrate s))

-- $generic
-- Generic list rotations such that @rotUp [1..4]@ is equivalent to
-- @[2,3,4,1]@ and @rotDown [1..4]@ to @[4,1,2,3]@. They both are
-- @id@ for null or singleton lists.
rotUp :: [a] -> [a]
rotUp   l = drop 1 l ++ take 1 l
rotDown :: [a] -> [a]
rotDown = reverse . rotUp . reverse
