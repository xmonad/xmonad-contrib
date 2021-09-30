-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.ConfirmPrompt
-- Description :  A prompt for setting up simple confirmation prompts for keybindings.
-- Copyright   :  (C) 2015 Antoine Beaupré
-- License     :  BSD3
--
-- Maintainer  :  Antoine Beaupré <anarcat@debian.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for setting up simple confirmation prompts for keybindings.
--
-----------------------------------------------------------------------------
module XMonad.Prompt.ConfirmPrompt (confirmPrompt
                                    -- * Usage
                                    -- $usage
                                    , module XMonad.Prompt
                                    -- * Use case: confirming exit
                                    -- $tip
                                    , EnterPrompt
                                    ) where

import XMonad (X)
import XMonad.Prompt (XPConfig, XPrompt, showXPrompt, mkXPrompt, mkComplFunFromList)

{- $usage

This module makes it easy to add a confirmation prompt for specific
actions. Instead of just running the action, a simple confirmation
prompt will be created using 'XMonad.Prompt' primitives. The action
will then run normally if the user confirms.
-}

{- $tip
This should be used something like this:

> ...
> , ((modm , xK_l), confirmPrompt def "exit" $ io (exitWith ExitSuccess))
> ...
-}

{- | Customized 'XPrompt' prompt that will ask to confirm the given string -}
newtype EnterPrompt = EnterPrompt String
instance XPrompt EnterPrompt where
    showXPrompt (EnterPrompt n) = "Confirm " ++ n ++ " (esc/ENTER)"

{- | Prompt the user to confirm a given action. We offer no completion
     and simply ask to confirm (ENTER) or cancel (ESCAPE). The actual key
     handling is done by mkXPrompt.-}
confirmPrompt :: XPConfig -> String -> X() -> X()
confirmPrompt config app func = mkXPrompt (EnterPrompt app) config (mkComplFunFromList config []) $ const func
