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
                             xmonadPrompt
                              ) where
{-
usage:
in Config.hs add:
> import XMonadContrib.XPrompt
> import XMonadContrib.XMonadPrompt

in you keybindings add:

>   , ((modMask .|. controlMask, xK_x), xmonadPrompt defaultPromptConfig)

-}

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Commands

data XMonad = XMonad

instance XPrompt XMonad where
    showXPrompt XMonad = "XMonad:   "

xmonadPrompt :: XPConfig -> X ()
xmonadPrompt c = mkXPrompt XMonad c (mkComplFunFromList (map fst defaultCommands)) runCommand'
