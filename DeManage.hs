{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DeManage
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides a method to cease management of a window, without
-- unmapping it.  This is especially useful for applications like kicker and
-- gnome-panel.
--
-- To make a panel display correctly with xmonad:
--
--  * Determine the pixel size of the panel, add that value to defaultGaps
--  * Launch the panel
--  * Give the panel window focus, then press mod-d
--  * Convince the panel to move/resize to the correct location.  Changing the
--  panel's position setting several times seems to work.
--
-----------------------------------------------------------------------------

module XMonadContrib.DeManage (
                                 -- * Usage
                                 -- $usage
                                 demanage
                                ) where

import qualified StackSet as W
import XMonad
import Operations
import Control.Monad.State

-- $usage
-- To use demanage, add this import:
--
-- >     import XMonadContrib.GreedyView
--
-- And add a keybinding to it:
--
-- > , ((modMask,               xK_d     ), demanage)
--

-- | Stop managing the current focused window.
demanage :: X ()
demanage = do
    ws <- gets windowset
    modify (\s -> s { windowset = maybe ws (flip W.delete ws) (W.peek ws) })
    refresh
