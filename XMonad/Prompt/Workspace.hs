-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Workspace
-- Description :  A workspace prompt.
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A workspace prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Workspace (
                             -- * Usage
                             -- $usage
                             workspacePrompt,

                             -- * For developers
                             Wor(Wor),
                              ) where

import XMonad hiding ( workspaces )
import XMonad.Prompt
import XMonad.StackSet ( workspaces, tag )
import XMonad.Util.WorkspaceCompare ( getSortByIndex )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Workspace
--
-- >   , ((modm .|. shiftMask, xK_m     ), workspacePrompt def (windows . W.shift))
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype Wor = Wor String

instance XPrompt Wor where
    showXPrompt (Wor x) = x

workspacePrompt :: XPConfig -> (String -> X ()) -> X ()
workspacePrompt c job = do ws <- gets (workspaces . windowset)
                           sort <- getSortByIndex
                           let ts = map tag $ sort ws
                           mkXPrompt (Wor "") c (mkComplFunFromList' c ts) job
