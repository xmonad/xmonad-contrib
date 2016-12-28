--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.EZConfig
-- Copyright   :  Devin Mullins <me@twifkak.com>
--                Brent Yorgey <byorgey@gmail.com> (key parsing)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
--
-- Useful helper functions for amending the default configuration, and for
-- parsing keybindings specified in a special (emacs-like) format.
--
-- (See also "XMonad.Util.CustomKeys" in xmonad-contrib.)
--
--------------------------------------------------------------------

module XMonad.Util.EZConfig (
                             -- * Usage
                             -- $usage

                             -- * Adding or removing keybindings

                             additionalKeys, additionalKeysP,
                             removeKeys, removeKeysP,
                             additionalMouseBindings, removeMouseBindings,

                             -- * Emacs-style keybinding specifications

                             mkKeymap, checkKeymap,
                             mkNamedKeymap,

                             parseKey -- used by XMonad.Util.Paste
                            ) where

import XMonad
import XMonad.Keys.Parse (parseKey)
import qualified Data.Map as M

-- $usage
-- To use this module, first import it into your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Util.EZConfig
--
-- Then, use one of the provided functions to modify your
-- configuration.  You can use 'additionalKeys', 'removeKeys',
-- 'additionalMouseBindings', and 'removeMouseBindings' to easily add
-- and remove keybindings or mouse bindings.  You can use 'mkKeymap'
-- to create a keymap using emacs-style keybinding specifications
-- like @\"M-x\"@ instead of @(modMask, xK_x)@, or 'additionalKeysP'
-- and 'removeKeysP' to easily add or remove emacs-style keybindings.
-- If you use emacs-style keybindings, the 'checkKeymap' function is
-- provided, suitable for adding to your 'startupHook', which can warn
-- you of any parse errors or duplicate bindings in your keymap.
--
-- For more information and usage examples, see the documentation
-- provided with each exported function, and check the xmonad config
-- archive (<http://haskell.org/haskellwiki/Xmonad/Config_archive>)
-- for some real examples of use.

-- |
-- Add or override keybindings from the existing set. Example use:
--
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `additionalKeys`
-- >                 [ ((mod1Mask, xK_m        ), spawn "echo 'Hi, mom!' | dzen2 -p 4")
-- >                 , ((mod1Mask, xK_BackSpace), withFocused hide) -- N.B. this is an absurd thing to do
-- >                 ]
--
-- This overrides the previous definition of mod-m.
--
-- Note that, unlike in xmonad 0.4 and previous, you can't use modMask to refer
-- to the modMask you configured earlier. You must specify mod1Mask (or
-- whichever), or add your own @myModMask = mod1Mask@ line.
additionalKeys :: XConfig a -> [((KeyMask, KeySym), X ())] -> XConfig a
additionalKeys conf keyList =
    conf { keys = \cnf -> M.union (M.fromList keyList) (keys conf cnf) }

-- | Like 'additionalKeys', except using short @String@ key
--   descriptors like @\"M-m\"@ instead of @(modMask, xK_m)@, as
--   described in the documentation for 'mkKeymap'.  For example:
--
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `additionalKeysP`
-- >                 [ ("M-m", spawn "echo 'Hi, mom!' | dzen2 -p 4")
-- >                 , ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
-- >                 ]

additionalKeysP :: XConfig l -> [(String, X ())] -> XConfig l
additionalKeysP conf keyList =
    conf { keys = \cnf -> M.union (mkKeymap cnf keyList) (keys conf cnf) }

-- |
-- Remove standard keybindings you're not using. Example use:
--
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `removeKeys` [(mod1Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
removeKeys :: XConfig a -> [(KeyMask, KeySym)] -> XConfig a
removeKeys conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` M.fromList (zip keyList $ repeat ()) }

-- | Like 'removeKeys', except using short @String@ key descriptors
--   like @\"M-m\"@ instead of @(modMask, xK_m)@, as described in the
--   documentation for 'mkKeymap'. For example:
--
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `removeKeysP` ["M-S-" ++ [n] | n <- ['1'..'9']]

removeKeysP :: XConfig l -> [String] -> XConfig l
removeKeysP conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` mkKeymap cnf (zip keyList $ repeat (return ())) }

-- | Like 'additionalKeys', but for mouse bindings.
additionalMouseBindings :: XConfig a -> [((ButtonMask, Button), Window -> X ())] -> XConfig a
additionalMouseBindings conf mouseBindingsList =
    conf { mouseBindings = \cnf -> M.union (M.fromList mouseBindingsList) (mouseBindings conf cnf) }

-- | Like 'removeKeys', but for mouse bindings.
removeMouseBindings :: XConfig a -> [(ButtonMask, Button)] -> XConfig a
removeMouseBindings conf mouseBindingList =
    conf { mouseBindings = \cnf -> mouseBindings conf cnf `M.difference`
                                   M.fromList (zip mouseBindingList $ repeat ()) }
