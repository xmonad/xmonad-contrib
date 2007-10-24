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
import XMonadContrib.Run
import XMonadContrib.XPrompt

import System.Directory
import System.Environment

import Control.Monad
import Data.List
import Data.Maybe

-- $usage
-- 1. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.SshPrompt
--
-- 2. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_s), sshPrompt defaultXPConfig)
--

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.SshPrompt
-- %keybind , ((modMask .|. controlMask, xK_s), sshPrompt defaultXPConfig)

data Ssh = Ssh

instance XPrompt Ssh where
    showXPrompt Ssh = "SSH to: "

sshPrompt :: XPConfig -> X ()
sshPrompt c = do
  sc <- io $ sshComplList
  mkXPrompt Ssh c (mkComplFunFromList sc) ssh

ssh :: String -> X ()
ssh s = runInTerm ("ssh " ++ s)

sshComplList :: IO [String]
sshComplList = uniqSort `fmap` liftM2 (++) sshComplListLocal sshComplListGlobal

sshComplListLocal :: IO [String]
sshComplListLocal = do
  h <- getEnv "HOME"
  sshComplListFile $ h ++ "/.ssh/known_hosts"

sshComplListGlobal :: IO [String]
sshComplListGlobal = do
  env <- getEnv "SSH_KNOWN_HOSTS" `catch` (\_ -> return "/nonexistent")
  fs <- mapM fileExists [ env
                        , "/usr/local/etc/ssh/ssh_known_hosts"
                        , "/usr/local/etc/ssh_known_hosts"
                        , "/etc/ssh/ssh_known_hosts"
                        , "/etc/ssh_known_hosts"
                        ]
  case catMaybes fs of
    []    -> return []
    (f:_) -> sshComplListFile' f

sshComplListFile :: String -> IO [String]
sshComplListFile kh = do
  f <- doesFileExist kh
  if f then sshComplListFile' kh
       else return []

sshComplListFile' :: String -> IO [String]
sshComplListFile' kh = do
  l <- readFile kh
  return $ map (takeWhile (/= ',') . concat . take 1 . words)
         $ filter nonComment
         $ lines l

fileExists :: String -> IO (Maybe String)
fileExists kh = do
  f <- doesFileExist kh
  if f then return $ Just kh
       else return Nothing

nonComment :: String -> Bool
nonComment []      = False
nonComment ('#':_) = False
nonComment ('|':_) = False -- hashed, undecodeable
nonComment _       = True
