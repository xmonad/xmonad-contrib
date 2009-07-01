-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Layout
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
--
-- Maintainer  :  droundy@darcs.net
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

import Data.List ( sort, nub )
import XMonad hiding ( workspaces )
import XMonad.Prompt
import XMonad.StackSet ( workspaces, layout )
import XMonad.Layout.LayoutCombinators ( JumpToLayout(..) )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Layout
--
-- >   , ((modMask x .|. shiftMask, xK_m     ), layoutPrompt defaultXPConfig)
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

data Wor = Wor String

instance XPrompt Wor where
    showXPrompt (Wor x) = x

layoutPrompt :: XPConfig -> X ()
layoutPrompt c = do ls <- gets (map (description . layout) . workspaces . windowset)
                    mkXPrompt (Wor "") c (mkComplFunFromList' $ sort $ nub ls) (sendMessage . JumpToLayout)
