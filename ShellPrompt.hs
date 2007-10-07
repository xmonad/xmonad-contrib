-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ShellPrompt
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

module XMonadContrib.ShellPrompt (
                             -- * Usage
                             -- $usage
                             shellPrompt
                             , rmPath
                             , split
                              ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Dmenu

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.IO
import System.Environment

-- $usage
--
-- 1. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.ShellPrompt
--
-- 2. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
--

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.ShellPrompt
-- %keybind , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)

data Shell = Shell

instance XPrompt Shell where
    showXPrompt Shell = "Run:   "


shellPrompt :: XPConfig -> X ()
shellPrompt c = mkXPrompt Shell c getShellCompl spawn

getShellCompl :: String -> IO [String]
getShellCompl s 
    | s /= "" && last s /= ' ' = do
  f <- fmap (lines . fromMaybe "") $ runProcessWithInput "/bin/bash" [] ("compgen -A file " ++ s ++ "\n")
  c <- commandCompletionFunction s
  hPutStrLn stdout s
  return $ map escape . sort . nub $ f ++ c
    | otherwise = return []

commandCompletionFunction :: String -> IO [String]
commandCompletionFunction str 
    | '/' `elem` str = return []
    | otherwise      = do
  p <- getEnv "PATH"
  cl p
    where
      cl     = liftM (nub . rmPath . concat) . mapM cmpl . split ':'  
      cmpl s = filter (isPrefixOf str) `fmap` getFileNames s

getFileNames :: FilePath -> IO [FilePath]
getFileNames fp = 
    getDirectoryContents fp `catch` \_ -> return []

rmPath :: [String] -> [String]
rmPath s = 
    map (reverse . fst . break  (=='/') . reverse) s

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (rest ls)
        where 
          (f,ls) = span (/=e) l
          rest s | s == [] = []
                 | otherwise = tail s

escape :: String -> String
escape []       = ""
escape (' ':xs) = "\\ " ++ escape xs
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem "\\@\"'#?$*()[]{};"
