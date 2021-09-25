-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DeManage
-- Description :  Cease management of a window without unmapping it.
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- This module provides a method to cease management of a window
-- without unmapping it.  This is especially useful for applications
-- like kicker and gnome-panel.  See also "XMonad.Hooks.ManageDocks" for
-- more a more automated solution.
--
-- To make a panel display correctly with xmonad:
--
--  * Determine the pixel size of the panel, add that value to
--    'XMonad.Core.XConfig.defaultGaps'
--
--  * Launch the panel
--
--  * Give the panel window focus, then press @mod-d@ (or whatever key
--    you have bound 'demanage' to)
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

-- $usage
-- To use demanage, add this import to your @~\/.xmonad\/xmonad.hs@:
--
-- >     import XMonad.Actions.DeManage
--
-- And add a keybinding, such as:
--
-- > , ((modm,               xK_d     ), withFocused demanage)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Stop managing the currently focused window.
demanage :: Window -> X ()
demanage w = do
    -- use modify to defeat automatic 'unmanage' calls.
    modify (\s -> s { windowset = W.delete w (windowset s) })
    refresh
