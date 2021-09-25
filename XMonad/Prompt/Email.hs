-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Email
-- Description :  A prompt for sending quick, one-line emails, via GNU \'mail\'.
-- Copyright   :  (c) 2007 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- A prompt for sending quick, one-line emails, via the standard GNU
-- \'mail\' utility (which must be in your $PATH).  This module is
-- intended mostly as an example of using "XMonad.Prompt.Input" to
-- build an action requiring user input.
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Email (
                            -- * Usage
                            -- $usage
                            emailPrompt
                           ) where

import XMonad.Core
import XMonad.Util.Run
import XMonad.Prelude (void)
import XMonad.Prompt
import XMonad.Prompt.Input

-- $usage
--
-- You can use this module by importing it, along with
-- "XMonad.Prompt", into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Email
--
-- and adding an appropriate keybinding, for example:
--
-- >  , ((modm .|. controlMask, xK_e), emailPrompt def addresses)
--
-- where @addresses@ is a list of email addresses that should
-- autocomplete, for example:
--
-- > addresses = ["me@me.com", "mr@big.com", "tom.jones@foo.bar"]
--
-- You can still send email to any address, but sending to these
-- addresses will be faster since you only have to type a few
-- characters and then hit \'tab\'.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".


-- | Prompt the user for a recipient, subject, and body, and send an
--   email via the GNU \'mail\' utility.  The second argument is a list
--   of addresses for autocompletion.
emailPrompt :: XPConfig -> [String] -> X ()
emailPrompt c addrs =
    inputPromptWithCompl c "To" (mkComplFunFromList c addrs) ?+ \to ->
    inputPrompt c "Subject" ?+ \subj ->
    inputPrompt c "Body" ?+ \body ->
    void (runProcessWithInput "mail" ["-s", subj, to] (body ++ "\n"))
