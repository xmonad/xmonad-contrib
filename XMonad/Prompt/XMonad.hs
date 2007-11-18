-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.XMonad
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A prompt for running XMonad commands
--
-----------------------------------------------------------------------------

module XMonad.Prompt.XMonad (
                             -- * Usage
                             -- $usage
                             xmonadPrompt,
                             xmonadPromptC
                              ) where

import XMonad
import XMonad.Prompt
import XMonad.Actions.Commands (defaultCommands, runCommand')

-- $usage
--
-- in Config.hs add:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.XMonad
--
-- in your keybindings add:
--
-- >   , ((modMask x .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
--

-- %import XMonad.Prompt
-- %import XMonad.Prompt.XMonad
-- %keybind , ((modMask x .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)

data XMonad = XMonad

instance XPrompt XMonad where
    showXPrompt XMonad = "XMonad: "

xmonadPrompt :: XPConfig -> X ()
xmonadPrompt c = do
    cmds <- defaultCommands
    mkXPrompt XMonad c (mkComplFunFromList (map fst cmds)) runCommand'

-- xmonad prompt with custom command list
xmonadPromptC :: [(String, X ())] -> XPConfig -> X ()
xmonadPromptC commands c = mkXPrompt XMonad c (mkComplFunFromList (map fst commands)) runCommand'
