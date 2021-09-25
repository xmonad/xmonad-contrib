-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Promote
-- Description :  Alternate promote function for xmonad.
-- Copyright   :  (c) Miikka Koskinen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  xmonad@s001.ethrael.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Alternate promote function for xmonad.
--
-- Moves the focused window to the master pane. All other windows
-- retain their order. If focus is in the master, swap it with the
-- next window in the stack. Focus stays in the master.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Promote (
                                 -- * Usage
                                 -- $usage
                                 promote
                                ) where

import XMonad
import XMonad.StackSet

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.Promote
--
-- then add a keybinding or substitute 'promote' in place of swapMaster:
--
-- >   , ((modm,               xK_Return), promote)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Move the focused window to the master pane. All other windows
--   retain their order. If focus is in the master, swap it with the
--   next windo in the stack. Focus stays in the master.
promote :: X ()
promote = windows $ modify' $
          \c -> case c of
                Stack _ [] []     -> c
                Stack t [] (x:rs) -> Stack x [] (t:rs)
                Stack t ls rs     -> Stack t [] (reverse ls ++ rs)
