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
-- Focus the nth window of the current workspace.
-----------------------------------------------------------------------------

module XMonad.Actions.FocusNth (
                 -- * Usage
                 -- $usage
                 focusNth,focusNth') where

import XMonad.StackSet
import XMonad

-- $usage
-- Add the import to your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.FocusNth
--
-- Then add appropriate keybindings, for example:
--
-- > -- mod4-[1..9] @@ Switch to window N
-- > ++ [((modMask x, k), focusNth i)
-- >     | (i, k) <- zip [0 .. 8] [xK_1 ..]]
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Give focus to the nth window of the current workspace.
focusNth :: Int -> X ()
focusNth = windows . modify' . focusNth'

focusNth' :: Int -> Stack a -> Stack a
focusNth' n s@(Stack _ ls rs) | (n < 0) || (n > length(ls) + length(rs)) = s
                              | otherwise = listToStack n (integrate s)

listToStack :: Int -> [a] -> Stack a
listToStack n l = Stack t ls rs
 where
    (t:rs)    = drop n l
    ls        = reverse (take n l)


