-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.FocusNth
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Focus the nth window on the screen.
-----------------------------------------------------------------------------

module XMonad.Actions.FocusNth (
                 -- * Usage
                 -- $usage
                 focusNth) where

import XMonad.StackSet
import XMonad.Operations
import XMonad

-- $usage
-- > import XMonad.Actions.FocusNth

-- > -- mod4-[1..9] @@ Switch to window N
-- > ++ [((mod4Mask, k), focusNth i)
-- >     | (i, k) <- zip [0 .. 8] [xK_1 ..]]

-- %import XMonad.Actions.FocusNth
-- %keybdindextra ++
-- %keybdindextra -- mod4-[1..9] @@ Switch to window N
-- %keybdindextra [((mod4Mask, k), focusNth i)
-- %keybdindextra     | (i, k) <- zip [0 .. 8] [xK_1 ..]]

focusNth :: Int -> X ()
focusNth = windows . modify' . focusNth'

focusNth' :: Int -> Stack a -> Stack a
focusNth' n s@(Stack _ ls rs)	| (n < 0) || (n > length(ls) + length(rs)) = s
				| otherwise = listToStack n (integrate s)

listToStack :: Int -> [a] -> Stack a
listToStack n l = Stack t ls rs
	where	(t:rs)	= drop n l
		ls	= reverse (take n l)


