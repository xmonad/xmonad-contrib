-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SimpleDate
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

module XMonadContrib.SimpleDate (
                                 -- * Usage
                                 -- $usage
                                 date
                                ) where

import XMonad

-- $usage
-- To use, modify your Config.hs to:
--
-- >     import XMonadContrib.SimpleDate
--
-- and add a keybinding:
--
-- >    , ((modMask,               xK_d     ), date)
--
-- a popup date menu will now be bound to mod-d

-- %import XMonadContrib.SimpleDate
-- %keybind , ((modMask,               xK_d     ), date)

date :: X ()
date = spawn "(date; sleep 10) | dzen2"
