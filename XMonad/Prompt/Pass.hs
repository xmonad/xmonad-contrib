-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Pass
-- Description :  A prompt for interacting with @pass(1)@.
-- Copyright   :  (c) 2014 Igor Babuschkin, Antoine R. Dumont
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Antoine R. Dumont <eniotna.t@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A thin wrapper around the standard @pass(1)@ UNIX utility.
--
-- This module provides several prompts to ease password manipulation
-- (generate, read, edit, remove); all of them benefit from the
-- completion system provided by "XMonad.Prompt".  Specifically, we
-- provide
--
-- - two functions to lookup passwords in the password-store:
--
--     - 'passPrompt' copies the password directly to the clipboard.
--
--     - 'passTypePrompt' uses @xdotool@ to type the password
--       directly.
--
-- - 'passGeneratePrompt' generates a password for a given password
--   label that the user inputs.
--
-- - 'passEditPrompt' edits a password for a given password label that
--   the user inputs.
--
-- - 'passRemovePrompt' deletes a stored password for a given password
--   label that the user inputs.
--
-- The password store is setup through an environment variable
-- @$PASSWORD_STORE_DIR@, or @$HOME\/.password-store@ if it is unset.
-- The editor is determined from the environment variable @$EDITOR@.
--
-- Source:
--
-- - The <https://www.passwordstore.org/ password store>
--   implementation is <http://git.zx2c4.com/password-store here>.
--
-- - Inspired by <http://babushk.in/posts/combining-xmonad-and-pass.html>
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Pass
    ( -- * Usage
      -- $usage

      -- * Retrieving passwords
      passPrompt
    , passTypePrompt

      -- * Editing passwords
    , passEditPrompt
    , passRemovePrompt
    , passGeneratePrompt
    , passGenerateAndCopyPrompt

      -- * Misc
    , passOTPPrompt
    ) where

import System.Directory (getHomeDirectory)
import System.FilePath (combine, dropExtension, takeExtension)
import System.Posix.Env (getEnv)
import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , searchPredicate)
import XMonad.Util.Run (runProcessWithInput)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.Pass
--
-- Then add a keybinding for 'passPrompt', 'passGeneratePrompt',
-- 'passRemovePrompt', 'passEditPrompt' or 'passTypePrompt':
--
-- >   , ((modMask , xK_p)                              , passPrompt def)
-- >   , ((modMask .|. controlMask, xK_p)               , passGeneratePrompt def)
-- >   , ((modMask .|. shiftMask, xK_p)                 , passEditPrompt def)
-- >   , ((modMask .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt def)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
--
-- - how to setup the password store, see <http://git.zx2c4.com/password-store/about/>
--   or @man 1 pass@.
--

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in @$HOME/.password-store@.
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the @$PASSWORD_STORE_DIR@ environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = fmap passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory.
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (getPassCompl passwords $ searchPredicate xpconfig) passwordFunction

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

-- | A prompt to retrieve a OTP from a given entry.  Note that you will
-- need to use the <https://github.com/tadfisher/pass-otp pass-otp>
-- extension for this to work.
--
passOTPPrompt :: XPConfig -> X ()
passOTPPrompt = mkPassPrompt "Select OTP" selectOTP

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = mkPassPrompt "Generate password" generatePassword

-- | A prompt to generate a password for a given entry and immediately copy it
-- to the clipboard.  This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGenerateAndCopyPrompt :: XPConfig -> X ()
passGenerateAndCopyPrompt = mkPassPrompt "Generate and copy password" generateAndCopyPassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

-- | A prompt to type in a password for a given entry.
-- This doesn't touch the clipboard.
--
passTypePrompt :: XPConfig -> X ()
passTypePrompt = mkPassPrompt "Type password" typePassword

-- | A prompt to edit a given entry.
-- This doesn't touch the clipboard.
--
passEditPrompt :: XPConfig -> X ()
passEditPrompt = mkPassPrompt "Edit password" editPassword

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword passLabel = spawn $ "pass --clip \"" ++ escapeQuote passLabel ++ "\""

-- | Select an OTP.
--
selectOTP :: String -> X ()
selectOTP passLabel = spawn $ "pass otp --clip \"" ++ escapeQuote passLabel ++ "\""

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = spawn $ "pass generate --force \"" ++ escapeQuote passLabel ++ "\" 30"

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
-- After generating the password, it is copied to the clipboard.
--
generateAndCopyPassword :: String -> X ()
generateAndCopyPassword passLabel = spawn $ "pass generate --force -c \"" ++ escapeQuote passLabel ++ "\" 30"

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword passLabel = spawn $ "pass rm --force \"" ++ escapeQuote passLabel ++ "\""

-- | Edit a password stored for a given entry.
--
editPassword :: String -> X ()
editPassword passLabel = spawn $ "pass edit \"" ++ escapeQuote passLabel ++ "\""

-- | Type a password stored for a given entry using xdotool.
--
typePassword :: String -> X ()
typePassword passLabel = spawn $ "pass \"" ++ escapeQuote passLabel
  ++ "\"|head -n1|tr -d '\n'|xdotool type --clearmodifiers --file -"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]

-- | Retrieve the list of passwords from the password store 'passwordStoreDir
--
getPasswords :: FilePath -> IO [String]
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" [
    "-L", -- Traverse symlinks
    passwordStoreDir,
    "-type", "f",
    "-name", "*.gpg",
    "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
