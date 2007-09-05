-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SshPrompt
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A ssh prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonadContrib.SshPrompt (
                             -- * Usage
                             -- $usage
                             sshPrompt
                              ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.RunInXTerm

import Control.Monad
import System.Directory
import System.Environment

-- $usage
-- 1. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.SshPrompt
--
-- 3. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
--

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.SshPrompt
-- %keybind , ((modMask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)

data Ssh = Ssh

instance XPrompt Ssh where
    showXPrompt Ssh = "SSH to:   "

sshPrompt :: XPConfig -> X ()
sshPrompt c = do
  sc <- io $ sshComplList
  mkXPrompt Ssh c (mkComplFunFromList sc) ssh

ssh :: String -> X ()
ssh s = runInXTerm ("ssh " ++ s)
 
sshComplList :: IO [String]
sshComplList = do
  h <- getEnv "HOME"
  let kh = h ++ "/.ssh/known_hosts"
  f <- doesFileExist kh
  if f then do l <- readFile kh
               return $ map (takeWhile (/= ',') . concat . take 1 . words) (lines l)
       else return []
