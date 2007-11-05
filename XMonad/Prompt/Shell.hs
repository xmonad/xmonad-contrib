-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Shell
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A shell prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Shell(
                             -- * Usage
                             -- $usage
                             shellPrompt
                             , getShellCompl
                             , split
                             , prompt
                             , safePrompt
                              ) where

import System.Environment
import Control.Monad
import Data.List
import System.Directory
import System.IO
import XMonad.Util.Run
import XMonad hiding (config)
import XMonad.Prompt

-- $usage
--
-- 1. In Config.hs add:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Shell
--
-- 2. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
--

-- %import XMonad.Prompt
-- %import XMonad.Prompt.ShellPrompt
-- %keybind , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)

data Shell = Shell

instance XPrompt Shell where
    showXPrompt Shell = "Run: "

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
    cmds <- io $ getCommands
    mkXPrompt Shell c (getShellCompl cmds) spawn

-- | See safe and unsafeSpawn. prompt is an alias for safePrompt;
-- safePrompt and unsafePrompt work on the same principles, but will use
-- XPrompt to interactively query the user for input; the appearance is
-- set by passing an XPConfig as the second argument. The first argument
-- is the program to be run with the interactive input.
-- You would use these like this:
--
-- >     , ((modMask,               xK_b     ), safePrompt "firefox" greenXPConfig)
-- >     , ((modMask .|. shiftMask, xK_c     ), prompt ("xterm" ++ " -e") greenXPConfig)
--
-- Note that you want to use safePrompt for Firefox input, as Firefox
-- wants URLs, and unsafePrompt for the XTerm example because this allows
-- you to easily start a terminal executing an arbitrary command, like
-- 'top'.
prompt, unsafePrompt, safePrompt :: FilePath -> XPConfig -> X ()
prompt = unsafePrompt
safePrompt c config = mkXPrompt Shell config (getShellCompl [c]) run
    where run = safeSpawn c
unsafePrompt c config = mkXPrompt Shell config (getShellCompl [c]) run
    where run a = unsafeSpawn $ c ++ " " ++ a

getShellCompl :: [String] -> String -> IO [String]
getShellCompl cmds s | s == "" || last s == ' ' = return []
                     | otherwise                = do
    f <- fmap lines $ runProcessWithInput "bash" [] ("compgen -A file " ++ s ++ "\n")
    return . map escape . uniqSort $ f ++ commandCompletionFunction cmds s

commandCompletionFunction :: [String] -> String -> [String]
commandCompletionFunction cmds str | '/' `elem` str = []
                                   | otherwise      = filter (isPrefixOf str) cmds

getCommands :: IO [String]
getCommands = do
    p  <- getEnv "PATH" `catch` const (return [])
    let ds = split ':' p
        fp d f = d ++ "/" ++ f
    es <- forM ds $ \d -> do
        exists <- doesDirectoryExist d
        if exists
            then getDirectoryContents d >>= filterM (isExecutable . fp d)
            else return []
    return . uniqSort . concat $ es

isExecutable :: FilePath ->IO Bool
isExecutable f = do
    fe <- doesFileExist f
    if fe
        then fmap executable $ getPermissions f
        else return False

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
escape (' ':xs) = "\\ " ++ escape xs
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem "\\@\"'#?$*()[]{};"
