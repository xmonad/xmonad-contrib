-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Layout
-- Description :  A layout-selection prompt.
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout-selection prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Layout (
                             -- * Usage
                             -- $usage
                             layoutPrompt
                            ) where

import XMonad.Prelude ( sort, nub )
import XMonad hiding ( workspaces )
import XMonad.Prompt
import XMonad.Prompt.Workspace ( Wor(..) )
import XMonad.StackSet ( workspaces, layout )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Layout
--
-- >   , ((modm .|. shiftMask, xK_m     ), layoutPrompt def)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- WARNING: This prompt won't display all possible layouts, because the
-- code to enable this was rejected from xmonad core.  It only displays
-- layouts that are actually in use.  Also, you can only select layouts if
-- you are using NewSelect, rather than the Select defined in xmonad core
-- (which doesn't have this feature).  So all in all, this module is really
-- more a proof-of-principle than something you can actually use
-- productively.

layoutPrompt :: XPConfig -> X ()
layoutPrompt c = do ls <- gets (map (description . layout) . workspaces . windowset)
                    mkXPrompt (Wor "") c (mkComplFunFromList' c $ sort $ nub ls) (sendMessage . JumpToLayout)
