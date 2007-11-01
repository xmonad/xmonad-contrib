-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DeManage
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
--
--  * Launch the panel
--
--  * Give the panel window focus, then press mod-d
--
--  * Convince the panel to move\/resize to the correct location.  Changing the
--  panel's position setting several times seems to work.
--
-----------------------------------------------------------------------------

module XMonad.Actions.DeManage (
                                 -- * Usage
                                 -- $usage
                                 demanage
                                ) where

import qualified XMonad.StackSet as W
import XMonad
import XMonad.Operations
import Control.Monad.State
import Graphics.X11 (Window)

-- $usage
-- To use demanage, add this import:
--
-- >     import XMonad.Actions.DeManage
--
-- And add a keybinding to it:
--
-- > , ((modMask,               xK_d     ), withFocused demanage)
--

-- %import XMonad.Actions.DeManage
-- %keybind , ((modMask,               xK_d     ), withFocused demanage)

-- | Stop managing the current focused window.
demanage :: Window -> X ()
demanage w = do
    -- use modify to defeat automatic 'unmanage' calls.
    modify (\s -> s { windowset = W.delete w (windowset s) })
    refresh
