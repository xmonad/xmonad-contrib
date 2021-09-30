-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Input
-- Description :  Prompt the user for input and pass it along to some other action.
-- Copyright   :  (c) 2007 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- A generic framework for prompting the user for input and passing it
-- along to some other action.
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Input (
                            -- * Usage
                            -- $usage
                            inputPrompt,
                            inputPromptWithCompl,
                            (?+),
                            InputPrompt,
                           ) where

import XMonad.Core
import XMonad.Prompt

-- $usage
--
-- To use this module, import it along with "XMonad.Prompt":
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Input
--
-- This module provides no useful functionality in isolation, but
-- is intended for use in building other actions which require user
-- input.
--
-- For example, suppose Mr. Big wants a way to easily fire his
-- employees. We'll assume that he already has a function
--
-- > fireEmployee :: String -> X ()
--
-- which takes as input the name of an employee, and fires them.  He
-- just wants a convenient way to provide the input for this function
-- from within xmonad.  Here is where the "XMonad.Prompt.Input" module
-- comes into play.  He can use the 'inputPrompt' function to create a
-- prompt, and the '?+' operator to compose the prompt with the
-- @fireEmployee@ action, like so:
--
-- > firingPrompt :: X ()
-- > firingPrompt = inputPrompt def "Fire" ?+ fireEmployee
--
-- If @employees@ contains a list of all his employees, he could also
-- create an autocompleting version, like this:
--
-- > firingPrompt' = inputPromptWithCompl def "Fire"
-- >                     (mkComplFunFromList employees) ?+ fireEmployee
--
-- Now all he has to do is add a keybinding to @firingPrompt@ (or
-- @firingPrompt'@), such as
--
-- >  , ((modm .|. controlMask, xK_f),  firingPrompt)
--
-- Now when Mr. Big hits mod-ctrl-f, a prompt will pop up saying
-- \"Fire: \", waiting for him to type the name of someone to fire.
-- If he thinks better of it after hitting mod-ctrl-f and cancels the
-- prompt (e.g. by hitting Esc), the @fireEmployee@ action will not be
-- invoked.
--
-- (For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".)
--
-- "XMonad.Prompt.Input" is also intended to ease the process of
-- developing other modules which require user input. For an example
-- of a module developed using this functionality, see
-- "XMonad.Prompt.Email", which prompts the user for a recipient,
-- subject, and one-line body, and sends a quick email.

newtype InputPrompt = InputPrompt String

instance XPrompt InputPrompt  where
    showXPrompt (InputPrompt s) = s ++ ": "

-- | Given a prompt configuration and some prompt text, create an X
--   action which pops up a prompt waiting for user input, and returns
--   whatever they type.  Note that the type of the action is @X
--   (Maybe String)@, which reflects the fact that the user might
--   cancel the prompt (resulting in @Nothing@), or enter an input
--   string @s@ (resulting in @Just s@).
inputPrompt :: XPConfig -> String -> X (Maybe String)
inputPrompt c p = inputPromptWithCompl c p (const (return []))

-- | The same as 'inputPrompt', but with a completion function.  The
--   type @ComplFunction@ is @String -> IO [String]@, as defined in
--   "XMonad.Prompt".  The 'mkComplFunFromList' utility function, also
--   defined in "XMonad.Prompt", is useful for creating such a
--   function from a known list of possibilities.
inputPromptWithCompl :: XPConfig -> String -> ComplFunction -> X (Maybe String)
inputPromptWithCompl c p compl = mkXPromptWithReturn (InputPrompt p) c compl return


infixr 1 ?+

-- | A combinator for hooking up an input prompt action to a function
--   which can take the result of the input prompt and produce another
--   action. If the user cancels the input prompt, the
--   second function will not be run.
--
--   The astute student of types will note that this is actually a
--   very general combinator and has nothing in particular to do
--   with input prompts.  If you find a more general use for it and
--   want to move it to a different module, be my guest.
(?+) :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
x ?+ k = x >>= maybe (return ()) k
