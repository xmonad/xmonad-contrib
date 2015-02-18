-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Pass
-- Copyright   :  (c) 2014 Igor Babuschkin, Antoine R. Dumont
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Antoine R. Dumont <eniotna.t@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides 3 <XMonad.Prompt> to ease passwords manipulation (generate, read, remove):
--
-- - one to lookup passwords in the password-storage.
--
-- - one to generate a password for a given password label that the user inputs.
--
-- - one to delete a stored password for a given password label that the user inputs.
--
-- All those prompts benefit from the completion system provided by the module <XMonad.Prompt>.
--
-- The password store is setuped through an environment variable PASSWORD_STORE_DIR.
-- If this is set, use the content of the variable.
-- Otherwise, the password store is located on user's home @$HOME\/.password-store@.
--
--
-- Source:
--
-- - The password storage implementation is <http://git.zx2c4.com/password-store the password-store cli>.
--
-- - Inspired from <http://babushk.in/posts/combining-xmonad-and-pass.html>
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Pass (
                            -- * Usages
                            -- $usages
                              passPrompt
                            , passGeneratePrompt
                            , passRemovePrompt
                            ) where

import Control.Monad (liftM)
import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , mkComplFunFromList)
import System.Directory (getDirectoryContents, getHomeDirectory)
import System.FilePath (takeBaseName, takeExtension, dropExtension, combine)
import System.Posix.Env (getEnv)
import XMonad.Util.Run (runProcessWithInput)

-- $usages
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.Pass
--
-- Then add a keybinding for 'passPrompt', 'passGeneratePrompt' or 'passRemovePrompt':
--
-- >   , ((modMask x , xK_p)                              , passPrompt xpconfig)
-- >   , ((modMask x .|. controlMask, xK_p)               , passGeneratePrompt xpconfig)
-- >   , ((modMask x .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt xpconfig)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
--
-- - how to setup the password storage, see <http://git.zx2c4.com/password-store/about/>
--

type PromptLabel = String

data Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = liftM passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (mkComplFunFromList passwords) passwordFunction

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = mkPassPrompt "Generate password" generatePassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword passLabel = spawn $ "pass --clip " ++ passLabel

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = spawn $ "pass generate --force " ++ passLabel ++ " 30"

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword passLabel = spawn $ "pass rm --force " ++ passLabel

-- | Retrieve the list of passwords from the password storage 'passwordStoreDir
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" [
    passwordStoreDir,
    "-type", "f",
    "-name", "*.gpg",
    "-printf", "%P\n"] []
  return $ map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
