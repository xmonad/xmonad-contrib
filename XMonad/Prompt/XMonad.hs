-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.XMonad
-- Description :  A prompt for running XMonad commands.
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
                             xmonadPromptC,
                             xmonadPromptCT,
                             XMonad,
                              ) where

import XMonad
import XMonad.Prompt
import XMonad.Actions.Commands (defaultCommands)
import XMonad.Prelude (fromMaybe)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.XMonad
--
-- in your keybindings add:
--
-- >   , ((modm .|. controlMask, xK_x), xmonadPrompt def)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype XMonad = XMonad String

instance XPrompt XMonad where
    showXPrompt (XMonad str) = str <> ": "

xmonadPrompt :: XPConfig -> X ()
xmonadPrompt c = do
    cmds <- defaultCommands
    xmonadPromptC cmds c

-- | An xmonad prompt with a custom command list
xmonadPromptC :: [(String, X ())] -> XPConfig -> X ()
xmonadPromptC = xmonadPromptCT "XMonad"

-- | An xmonad prompt with a custom command list and a custom title
xmonadPromptCT :: String -> [(String, X ())] -> XPConfig -> X ()
xmonadPromptCT title' commands c =
    mkXPrompt (XMonad title') c (mkComplFunFromList' c (map fst commands)) $
        fromMaybe (return ()) . (`lookup` commands)
