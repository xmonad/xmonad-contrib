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
                              ) where

import XMonad
import XMonadContrib.XPrompt

import Control.Monad
import Data.List
import System.Console.Readline
import System.Environment

-- $usage
--
-- 1. In xmonad.cabal change: 
--
-- > build-depends:      base>=2.0, X11>=1.2.1, X11-extras>=0.2, mtl>=1.0, unix>=1.0
--
-- to
--
-- > build-depends:      base>=2.0, X11>=1.2.1, X11-extras>=0.2, mtl>=1.0, unix>=1.0, readline >= 1.0
--
-- 2. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.ShellPrompt
--
-- 3. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
--

data Shell = Shell

instance XPrompt Shell where
    showXPrompt Shell = "Run:   "


shellPrompt :: XPConfig -> X ()
shellPrompt c = mkXPrompt Shell c getShellCompl spawn

getShellCompl :: String -> IO [String]
getShellCompl s 
    | s /= "" && last s /= ' ' = do
  fl <- filenameCompletionFunction s
  c <- commandCompletionFunction s
  return $ sort . nub $ fl ++ c
    | otherwise = return []

commandCompletionFunction :: String -> IO [String]
commandCompletionFunction str 
    | '/' `elem` str = return []
    | otherwise = do
  p <- getEnv "PATH"
  cl p
    where
      cl = liftM (nub . rmPath . concat) . mapM fCF . map addToPath . split ':'  
      addToPath = flip (++) ("/" ++ str)
      fCF = filenameCompletionFunction
      rmPath [] = []
      rmPath s = map (last . split '/') s

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (rest ls)
        where 
          (f,ls) = span (/=e) l
          rest s | s == [] = []
                 | otherwise = tail s

