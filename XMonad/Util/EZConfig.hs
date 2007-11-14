--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.EZConfig
-- Copyright   :  Devin Mullins <me@twifkak.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
--
-- Useful helper functions for amending the defaultConfig.
--
-- (See also "XMonad.Util.CustomKeys" in xmonad-contrib.)
--
--------------------------------------------------------------------

module XMonad.Util.EZConfig (
                             additionalKeys, removeKeys,
                             additionalMouseBindings, removeMouseBindings
                            ) where
-- TODO: write tests

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib

-- |
-- Add or override keybindings from the existing set. Example use:
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `additionalKeys`
-- >                 [ ((mod1Mask, xK_m        ), spawn "echo 'Hi, mom!' | dzen2 -p 4")
-- >                 , ((mod1Mask, xK_BackSpace), withFocused hide) -- N.B. this is an absurd thing to do
-- >                 ]
-- This overrides the previous definition of mod-m.
--
-- Note that, unlike in xmonad 0.4 and previous, you can't use modMask to refer
-- to the modMask you configured earlier. You must specify mod1Mask (or
-- whichever), or add your own @myModMask = mod1Mask@ line.
additionalKeys :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a
additionalKeys conf keysList =
    conf { keys = \cnf -> M.union (M.fromList keysList) (keys conf cnf) }

-- |
-- Remove standard keybidings you're not using. Example use:
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `removeKeys` [(mod1Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
removeKeys :: XConfig a -> [(ButtonMask, KeySym)] -> XConfig a
removeKeys conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` M.fromList (zip keyList $ return ()) }

-- | Like additionalKeys, but for mouseBindings.
additionalMouseBindings :: XConfig a -> [((ButtonMask, Button), Window -> X ())] -> XConfig a
additionalMouseBindings conf mouseBindingsList =
    conf { mouseBindings = \cnf -> M.union (M.fromList mouseBindingsList) (mouseBindings conf cnf) }

-- | Like removeKeys, but for mouseBindings.
removeMouseBindings :: XConfig a -> [(ButtonMask, Button)] -> XConfig a
removeMouseBindings conf mouseBindingList =
    conf { mouseBindings = \cnf -> mouseBindings conf cnf `M.difference`
                                   M.fromList (zip mouseBindingList $ return ()) }
