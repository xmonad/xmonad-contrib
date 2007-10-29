-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.XMonadPrompt
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

module XMonadContrib.XMonadPrompt (
                             -- * Usage
                             -- $usage
                             xmonadPrompt,
			     xmonadPromptC
                              ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Commands (defaultCommands, runCommand')

-- $usage
--
-- in Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.XMonadPrompt
--
-- in you keybindings add:
--
-- >   , ((modMask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
--

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.XMonadPrompt
-- %keybind , ((modMask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)

data XMonad = XMonad

instance XPrompt XMonad where
    showXPrompt XMonad = "XMonad: "

xmonadPrompt :: XPConfig -> X ()
xmonadPrompt c = mkXPrompt XMonad c (mkComplFunFromList (map fst defaultCommands)) runCommand'

-- xmonad prompt with custom command list
xmonadPromptC :: [(String, X ())] -> XPConfig -> X ()
xmonadPromptC commands c = mkXPrompt XMonad c (mkComplFunFromList (map fst commands)) runCommand'
