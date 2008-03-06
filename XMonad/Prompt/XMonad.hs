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
import XMonad.Actions.Commands (defaultCommands)
import Data.Maybe (fromMaybe)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.XMonad
--
-- in your keybindings add:
--
-- >   , ((modMask x .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data XMonad = XMonad

instance XPrompt XMonad where
    showXPrompt XMonad = "XMonad: "

xmonadPrompt :: XPConfig -> X ()
xmonadPrompt c = do
    cmds <- defaultCommands
    xmonadPromptC cmds c

-- | An xmonad prompt with a custom command list
xmonadPromptC :: [(String, X ())] -> XPConfig -> X ()
xmonadPromptC commands c =
    mkXPrompt XMonad c (mkComplFunFromList' (map fst commands)) $
        fromMaybe (return ()) . (`lookup` commands)
