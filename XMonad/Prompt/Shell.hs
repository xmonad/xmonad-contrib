{- |
Module      :  XMonad.Prompt.Shell
Copyright   :  (C) 2007 Andrea Rossato
License     :  BSD3

Maintainer  :  andrea.rossato@unibz.it
Stability   :  unstable
Portability :  unportable

A shell prompt for XMonad
-}

module XMonad.Prompt.Shell
    ( -- * Usage
      -- $usage
      Shell (..)
    , shellPrompt
    -- ** Variations on shellPrompt
    -- $spawns
    , prompt
    , safePrompt
    , unsafePrompt

    -- * Utility functions
    , getCommands
    , getBrowser
    , getEditor
    , getShellCompl
    , split
    ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Exception
import Control.Monad (forM)
import Data.List (isPrefixOf)
import Prelude hiding (catch)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getEnv)
import System.Posix.Files (getFileStatus, isDirectory)

import XMonad.Util.Run
import XMonad hiding (config)
import XMonad.Prompt

econst :: Monad m => a -> IOException -> m a
econst = const . return

{- $usage
1. In your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Prompt
> import XMonad.Prompt.Shell

2. In your keybindings add something like:

>   , ((modm .|. controlMask, xK_x), shellPrompt defaultXPConfig)

For detailed instruction on editing the key binding see
"XMonad.Doc.Extending#Editing_key_bindings". -}

data Shell = Shell

instance XPrompt Shell where
    showXPrompt Shell     = "Run: "
    completionToCommand _ = escape

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
    cmds <- io getCommands
    mkXPrompt Shell c (getShellCompl cmds) spawn

{- $spawns
    See safe and unsafeSpawn in "XMonad.Util.Run".
    prompt is an alias for safePrompt;
    safePrompt and unsafePrompt work on the same principles, but will use
    XPrompt to interactively query the user for input; the appearance is
    set by passing an XPConfig as the second argument. The first argument
    is the program to be run with the interactive input.
    You would use these like this:

    >     , ((modm,               xK_b), safePrompt "firefox" greenXPConfig)
    >     , ((modm .|. shiftMask, xK_c), prompt ("xterm" ++ " -e") greenXPConfig)

    Note that you want to use safePrompt for Firefox input, as Firefox
    wants URLs, and unsafePrompt for the XTerm example because this allows
    you to easily start a terminal executing an arbitrary command, like
    'top'. -}

prompt, unsafePrompt, safePrompt :: FilePath -> XPConfig -> X ()
prompt = unsafePrompt
safePrompt c config = mkXPrompt Shell config (getShellCompl [c]) run
    where run = safeSpawn c . return
unsafePrompt c config = mkXPrompt Shell config (getShellCompl [c]) run
    where run a = unsafeSpawn $ c ++ " " ++ a

getShellCompl :: [String] -> String -> IO [String]
getShellCompl cmds s | s == "" || last s == ' ' = return []
                     | otherwise                = do
    f     <- fmap lines $ runProcessWithInput "bash" [] ("compgen -A file -- "
                                                        ++ s ++ "\n")
    files <- case f of
               [x] -> do fs <- getFileStatus (encodeString x)
                         if isDirectory fs then return [x ++ "/"]
                                           else return [x]
               _   -> return f
    return . uniqSort $ files ++ commandCompletionFunction cmds s

commandCompletionFunction :: [String] -> String -> [String]
commandCompletionFunction cmds str | '/' `elem` str = []
                                   | otherwise      = filter (isPrefixOf str) cmds

getCommands :: IO [String]
getCommands = do
    p  <- getEnv "PATH" `catch` econst []
    let ds = filter (/= "") $ split ':' p
    es <- forM ds $ \d -> do
        exists <- doesDirectoryExist d
        if exists
            then getDirectoryContents d
            else return []
    return . uniqSort . filter ((/= '.') . head) . concat $ es

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (rest ls)
        where
          (f,ls) = span (/=e) l
          rest s | s == []   = []
                 | otherwise = tail s

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"

-- | Ask the shell environment for the value of a variable in XMonad's environment, with a default value.
--   In order to /set/ an environment variable (eg. combine with a prompt so you can modify @$HTTP_PROXY@ dynamically),
--   you need to use 'System.Posix.putEnv'.
env :: String -> String -> IO String
env variable fallthrough = getEnv variable `catch` econst fallthrough

{- | Ask the shell what browser the user likes. If the user hasn't defined any
   $BROWSER, defaults to returning \"firefox\", since that seems to be the most
   common X web browser.
   Note that if you don't specify a GUI browser but a textual one, that'll be a problem
   as 'getBrowser' will be called by functions expecting to be able to just execute the string
   or pass it to a shell; so in that case, define $BROWSER as something like \"xterm -e elinks\"
   or as the name of a shell script doing much the same thing. -}
getBrowser :: IO String
getBrowser = env "BROWSER" "firefox"

-- | Like 'getBrowser', but should be of a text editor. This gets the $EDITOR variable, defaulting to \"emacs\".
getEditor :: IO String
getEditor = env "EDITOR" "emacs"
