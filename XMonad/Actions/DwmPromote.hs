-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DwmPromote
-- Copyright   :  (c) Miikka Koskinen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  arcatan@kapsi.fi
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dwm-like swap function for xmonad.
--
-- Swaps focused window with the master window. If focus is in the
-- master, swap it with the next window in the stack. Focus stays in the
-- master.
--
-----------------------------------------------------------------------------

module XMonad.Actions.DwmPromote (
                                 -- * Usage
                                 -- $usage
                                 dwmpromote
                                ) where

import XMonad
import XMonad.Operations (windows)
import XMonad.StackSet

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.DwmPromote
--
-- then add a keybinding or substitute 'dwmpromote' in place of promote:
--
-- >   , ((modMask,               xK_Return), dwmpromote)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Swap the focused window with the master window. If focus is in
--   the master, swap it with the next window in the stack. Focus
--   stays in the master.
dwmpromote :: X ()
dwmpromote = windows $ modify' $
             \c -> case c of
                   Stack _ [] []     -> c
                   Stack t [] (x:rs) -> Stack x [] (t:rs)
                   Stack t ls rs     -> Stack t [] (ys ++ x : rs) where (x:ys) = reverse ls
