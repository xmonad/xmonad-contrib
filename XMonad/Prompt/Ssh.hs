-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Ssh
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

module XMonad.Prompt.Ssh
    ( -- * Usage
      -- $usage
      sshPrompt,
      Ssh,
    ) where

import XMonad
import XMonad.Util.Run
import XMonad.Prompt

import System.Directory
import System.Environment
import Control.Exception as E

import Control.Applicative (liftA2)
import Data.Maybe
import Data.List(elemIndex)

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Ssh
--
-- 2. In your keybindings add something like:
--
-- >   , ((modm .|. controlMask, xK_s), sshPrompt def)
--
-- Keep in mind, that if you want to use the completion you have to
-- disable the "HashKnownHosts" option in your ssh_config
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Ssh = Ssh

instance XPrompt Ssh where
    showXPrompt       Ssh = "SSH to: "
    commandToComplete _ c = maybe c (\(_u,h) -> h) (parseHost c)
    nextCompletion _t c l = maybe next (\(u,_h) -> u ++ "@" ++ next) hostPared
                            where
                              hostPared = parseHost c
                              next = getNextCompletion (maybe c (\(_u,h) -> h) hostPared) l

sshPrompt :: XPConfig -> X ()
sshPrompt c = do
  sc <- io sshComplList
  mkXPrompt Ssh c (mkComplFunFromList c sc) ssh

ssh :: String -> X ()
ssh = runInTerm "" . ("ssh " ++ )

sshComplList :: IO [String]
sshComplList = uniqSort <$> liftA2 (++) sshComplListLocal sshComplListGlobal

sshComplListLocal :: IO [String]
sshComplListLocal = do
  h <- getEnv "HOME"
  s1 <- sshComplListFile $ h ++ "/.ssh/known_hosts"
  s2 <- sshComplListConf $ h ++ "/.ssh/config"
  return $ s1 ++ s2

sshComplListGlobal :: IO [String]
sshComplListGlobal = do
  env <- getEnv "SSH_KNOWN_HOSTS" `E.catch` econst "/nonexistent"
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
  return $ map (getWithPort . takeWhile (/= ',') . concat . take 1 . words)
         $ filter nonComment
         $ lines l

sshComplListConf :: String -> IO [String]
sshComplListConf kh = do
  f <- doesFileExist kh
  if f then sshComplListConf' kh
       else return []

sshComplListConf' :: String -> IO [String]
sshComplListConf' kh = do
  l <- readFile kh
  return $ map (!!1)
         $ filter isHost
         $ map words
         $ lines l
 where
   isHost ws = take 1 ws == ["Host"] && length ws > 1

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

getWithPort :: String -> String
getWithPort ('[':str) = host ++ " -p " ++ port
    where (host,p) = break (==']') str
          port = case p of
                   ']':':':x -> x
                   _         -> "22"
getWithPort  str = str

parseHost :: String -> Maybe (String, String)
parseHost a = elemIndex '@' a  >>= (\c-> Just ( (take c a), (drop (c+1) a) ) )
