{-# LANGUAGE LambdaCase #-}
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
-- - various functions to lookup passwords in the password-store:
--
--     + 'passPrompt' copies the password directly to the clipboard.
--
--     + 'passOTPPrompt' copies a one-time-password to the clipboard
--        (this uses <https://github.com/tadfisher/pass-otp pass-otp>).
--
--     + 'passTypePrompt' and 'passOTPTypePrompt' work like the above,
--       respectively, but use @xdotool@ to type out the password.
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
    , passPrompt'
    , passTypePrompt

      -- * Editing passwords
    , passEditPrompt
    , passEditPrompt'
    , passRemovePrompt
    , passRemovePrompt'
    , passGeneratePrompt
    , passGeneratePrompt'
    , passGenerateAndCopyPrompt
    , passGenerateAndCopyPrompt'

      -- * One-time-passwords
    , passOTPPrompt
    , passOTPTypePrompt
    ) where

import System.Directory (getHomeDirectory)
import System.FilePath (dropExtension, (</>))
import System.Posix.Env (getEnv)
import XMonad
import XMonad.Prelude
import XMonad.Prompt
  ( XPConfig,
    XPrompt,
    commandToComplete,
    getNextCompletion,
    mkXPrompt,
    nextCompletion,
    searchPredicate,
    showXPrompt,
  )
import XMonad.Util.Run (runProcessWithInput)

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
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
-- You can also use the versions that let you specify a custom prompt:
--
-- >   , ((modMask , xK_p)                              , passPrompt' "Ask 'pass' for" def)
--
-- Note that, by default, we do not use fuzzy matching in this module.
-- To enable this feature, import the "XMonad.Prompt.FuzzyMatch" module
-- and add the relevant functions to your prompt configuration:
--
-- > myXPConfig :: XPConfig
-- > myXPConfig = def
-- >   { searchPredicate = fuzzyMatch
-- >   , sorter          = fuzzySort
-- >   }
-- >
-- > , ((modMask , xK_p), passPrompt myXPConfig)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.
--
-- - how to setup the password store, see <http://git.zx2c4.com/password-store/about/>
--   or @man 1 pass@.
--

---------------------------------------------------------------------------------
-- Prompt

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = passPrompt' "Select password"

-- | The same as 'passPrompt' but with a user-specified prompt.
passPrompt' :: String -> XPConfig -> X ()
passPrompt' s = mkPassPrompt s selectPassword

-- | A prompt to retrieve a OTP from a given entry.  Note that you will
-- need to use the <https://github.com/tadfisher/pass-otp pass-otp>
-- extension for this to work.
--
passOTPPrompt :: XPConfig -> X ()
passOTPPrompt = mkPassPrompt "Select OTP" selectOTP

-- | A prompt to retrieve a OTP from a given entry.  Note that you will
-- need to use the <https://github.com/tadfisher/pass-otp pass-otp>
-- extension for this to work.
--
passOTPTypePrompt :: XPConfig -> X ()
passOTPTypePrompt = mkPassPrompt "Select OTP" selectOTPType

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = passGeneratePrompt' "Generate password"

-- | The same as 'passGeneratePrompt' but with a user-specified prompt.
passGeneratePrompt' :: String -> XPConfig -> X ()
passGeneratePrompt' s = mkPassPrompt s generatePassword

-- | A prompt to generate a password for a given entry and immediately copy it
-- to the clipboard.  This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGenerateAndCopyPrompt :: XPConfig -> X ()
passGenerateAndCopyPrompt = passGenerateAndCopyPrompt' "Generate and copy password"

-- | The same as 'passGenerateAndCopyPrompt' but with a user-specified prompt.
passGenerateAndCopyPrompt' :: String -> XPConfig -> X ()
passGenerateAndCopyPrompt' s = mkPassPrompt s generateAndCopyPassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = passRemovePrompt' "Remove password"

-- | The same as 'passRemovePrompt' but with a user-specified prompt.
passRemovePrompt' :: String -> XPConfig -> X ()
passRemovePrompt' s = mkPassPrompt s removePassword

-- | A prompt to type in a password for a given entry.
-- This doesn't touch the clipboard.
--
passTypePrompt :: XPConfig -> X ()
passTypePrompt = mkPassPrompt "Type password" typePassword

-- | A prompt to edit a given entry.
-- This doesn't touch the clipboard.
--
passEditPrompt :: XPConfig -> X ()
passEditPrompt = passEditPrompt' "Edit password"

-- | The same as 'passEditPrompt' but with a user-specified prompt.
passEditPrompt' :: String -> XPConfig -> X ()
passEditPrompt' s = mkPassPrompt s editPassword

-- | A pass prompt factory.
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel)
            xpconfig
            (getPassCompl passwords $ searchPredicate xpconfig)
            passwordFunction
 where
  getPassCompl :: [String] -> (String -> String -> Bool) -> String -> IO [String]
  getPassCompl compls p s = return $ filter (p s) compls

  -- Compute the password store's location. Use the @$PASSWORD_STORE_DIR@
  -- environment variable to set the password store. If empty, return the
  -- password store located in user's home.
  passwordStoreFolder :: IO String
  passwordStoreFolder =
    getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
   where
    -- Default password store folder in @$HOME/.password-store@.
    computePasswordStoreDir :: Maybe String -> IO String
    computePasswordStoreDir = \case
      Nothing       -> getHomeDirectory <&> (</> ".password-store")
      Just storeDir -> return storeDir

  -- Retrieve the list of passwords from the password store @passwordStoreDir@.
  getPasswords :: FilePath -> IO [String]
  getPasswords passwordStoreDir = do
    files <- runProcessWithInput "find" [
      "-L", -- Traverse symlinks
      passwordStoreDir,
      "-type", "f",
      "-name", "*.gpg",
      "-printf", "%P\n"] []
    return . map dropExtension $ lines files

---------------------------------------------------------------------------------
-- Selecting a password

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword = spawn . pass "--clip"

-- | Select a one-time-password and copy it to the clipboard.
--
selectOTP :: String -> X ()
selectOTP = spawn . pass "otp --clip"

-- | Select a one-time-password and type it out.
--
selectOTPType :: String -> X ()
selectOTPType = spawn . typeString . pass "otp"

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = spawn $ pass "generate --force" passLabel ++ " 30"

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
-- After generating the password, it is copied to the clipboard.
--
generateAndCopyPassword :: String -> X ()
generateAndCopyPassword passLabel = spawn $ pass "generate --force -c" passLabel ++ " 30"

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword = spawn . pass "rm --force"

-- | Edit a password stored for a given entry.
--
editPassword :: String -> X ()
editPassword = spawn . pass "edit"

-- | Type a password stored for a given entry using xdotool.
--
typePassword :: String -> X ()
typePassword = spawn . typeString . pass ""

-- | Type the given string with @xdotool@.
--
-- >>> typeString (pass "" "arXiv")
-- "pass  \"arXiv\" | head -n1 | tr -d '\n' | xdotool type --clearmodifiers --file -"
typeString :: String -> String
typeString cmd = cmd ++ " | head -n1 | tr -d '\n' | xdotool type --clearmodifiers --file -"

-- | Generate a pass prompt.
--
-- >>> pass "otp" "git\"hub\""
-- "pass otp \"git\\\"hub\\\"\""
pass :: String -> String -> String
pass cmd label = concat ["pass ", cmd, " \"", concatMap escape label, "\""]
 where
  escape :: Char -> String
  escape '"' = "\\\""
  escape x   = [x]
