{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}
{- |
Module      :  XMonad.Prompt.Shell
Description :  A shell prompt.
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
    , safePrompt
    , safeDirPrompt
    , unsafePrompt
    , prompt

    -- * Utility functions
    , compgenDirectories
    , compgenFiles
    , getCommands
    , getBrowser
    , getEditor
    , getShellCompl
    , getShellCompl'
    , split
    ) where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Exception        as E
import           Data.Bifunctor           (bimap)
import           System.Directory         (getDirectoryContents)
import           System.Environment       (getEnv)
import           System.Posix.Files       (getFileStatus, isDirectory)

import           XMonad                   hiding (config)
import           XMonad.Prelude
import           XMonad.Prompt
import           XMonad.Util.Run

econst :: Monad m => a -> IOException -> m a
econst = const . return

{- $usage
1. In your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Prompt
> import XMonad.Prompt.Shell

2. In your keybindings add something like:

>   , ((modm .|. controlMask, xK_x), shellPrompt def)

For detailed instruction on editing the key binding see
"XMonad.Doc.Extending#Editing_key_bindings". -}

data Shell = Shell
type Predicate = String -> String -> Bool

instance XPrompt Shell where
    showXPrompt Shell     = "Run: "
    completionToCommand _ = escape

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
    cmds <- io getCommands
    mkXPrompt Shell c (getShellCompl cmds $ searchPredicate c) spawn

{- $spawns
    See safe and unsafeSpawn in "XMonad.Util.Run".
    prompt is an alias for unsafePrompt;
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
safePrompt c config = mkXPrompt Shell config (getShellCompl [c] $ searchPredicate config) run
    where run = safeSpawn c . return
unsafePrompt c config = mkXPrompt Shell config (getShellCompl [c] $ searchPredicate config) run
    where run a = unsafeSpawn $ c ++ " " ++ a

{- | Like 'safePrompt', but optimized for the use-case of a program that
needs a file as an argument.

For example, a prompt for <https://github.com/mwh/dragon dragon> that
always starts searching in your home directory would look like

> safeDirPrompt "dragon" def "~/"

This is especially useful when using something like
'XMonad.Prompt.FuzzyMatch.fuzzyMatch' from "XMonad.Prompt.FuzzyMatch" as
your prompt's @searchPredicate@.
-}
safeDirPrompt
    :: FilePath  -- ^ The command to execute
    -> XPConfig  -- ^ The prompt configuration
    -> String    -- ^ Which string to start @compgen@ with
    -> X ()
safeDirPrompt cmd cfg@XPC{ searchPredicate } compgenStr =
    mkXPrompt Shell cfg mkCompl (safeSpawn cmd . pure)
  where
    mkCompl :: String -> IO [String]
    mkCompl input =
        shellComplImpl
            CaseSensitive
            (filter (searchPredicate ext))
            (commandCompletionFunction [cmd] searchPredicate input)
            (if "/" `isInfixOf` input then dir else compgenStr)
            input
      where
        -- "/path/to/some/file" ⇒ ("file", "/path/to/some/")
        (ext, dir) :: (String, String)
            = bimap reverse reverse . break (== '/') . reverse $ input

getShellCompl :: [String] -> Predicate -> String -> IO [String]
getShellCompl = getShellCompl' CaseSensitive

getShellCompl' :: ComplCaseSensitivity -> [String] -> Predicate -> String -> IO [String]
getShellCompl' csn cmds p input =
    shellComplImpl csn id (commandCompletionFunction cmds p input) input input

-- | Based in the user input and the given filtering function, create
-- the completion string to show in the prompt.
shellComplImpl
    :: ComplCaseSensitivity    -- ^ Whether the @compgen@ query should be case sensitive
    -> ([String] -> [String])  -- ^ How to filter the files we get back
    -> [String]                -- ^ The available commands to suggest
    -> String                  -- ^ Which string to give to @compgen@
    -> String                  -- ^ The input string
    -> IO [String]
shellComplImpl csn filterFiles cmds cmpgenStr input
    | input == "" || last input == ' ' = pure []
    | otherwise = do
        choices <- filterFiles . lines <$> compgenFiles csn cmpgenStr
        files   <- case choices of
            [x] -> do fs <- getFileStatus (encodeString x)
                      pure $ if isDirectory fs then [x ++ "/"] else [x]
            _   -> pure choices
        pure . sortBy typedFirst . uniqSort $ files ++ cmds
  where
    typedFirst :: String -> String -> Ordering
    typedFirst x y
        | x `startsWith` input && not (y `startsWith` input) = LT
        | y `startsWith` input && not (x `startsWith` input) = GT
        | otherwise = x `compare` y

    startsWith :: String -> String -> Bool
    startsWith str ps = map toLower ps `isPrefixOf` map toLower str

compgenFiles :: ComplCaseSensitivity -> String -> IO String
compgenFiles csn = compgen csn "file"

compgenDirectories :: ComplCaseSensitivity -> String -> IO String
compgenDirectories csn = compgen csn "directory"

compgen :: ComplCaseSensitivity -> String -> String -> IO String
compgen csn actionOpt s = runProcessWithInput "bash" [] $
    complCaseSensitivityCmd csn ++ " ; " ++ compgenCmd actionOpt s

complCaseSensitivityCmd :: ComplCaseSensitivity -> String
complCaseSensitivityCmd CaseSensitive =
    "bind 'set completion-ignore-case off'"
complCaseSensitivityCmd CaseInSensitive =
    "bind 'set completion-ignore-case on'"

compgenCmd :: String -> String -> String
compgenCmd actionOpt s = "compgen -A " ++ actionOpt ++ " -- " ++ s ++ "\n"

commandCompletionFunction :: [String] -> Predicate -> String -> [String]
commandCompletionFunction cmds p str | '/' `elem` str = []
                                     | otherwise      = filter (p str) cmds

getCommands :: IO [String]
getCommands = do
    p  <- getEnv "PATH" `E.catch` econst []
    let ds = filter (/= "") $ split ':' p
    es <- forM ds $ \d -> getDirectoryContents d `E.catch` econst []
    return . uniqSort . filter ((/= '.') . head) . concat $ es

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l

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
env variable fallthrough = getEnv variable `E.catch` econst fallthrough

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
