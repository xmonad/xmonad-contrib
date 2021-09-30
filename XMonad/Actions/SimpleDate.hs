-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.SimpleDate
-- Description :  An example external contrib module for XMonad.
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable
--
-- An example external contrib module for XMonad.
-- Provides a simple binding to dzen2 to print the date as a popup menu.
--
-----------------------------------------------------------------------------

module XMonad.Actions.SimpleDate (
                                 -- * Usage
                                 -- $usage
                                 date
                                ) where

import XMonad.Core
import XMonad.Util.Run

-- $usage
-- To use, import this module into @~\/.xmonad\/xmonad.hs@:
--
-- >     import XMonad.Actions.SimpleDate
--
-- and add a keybinding, for example:
--
-- >    , ((modm,               xK_d     ), date)
--
-- In this example, a popup date menu will now be bound to @mod-d@.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

date :: X ()
date = unsafeSpawn "(date; sleep 10) | dzen2"
