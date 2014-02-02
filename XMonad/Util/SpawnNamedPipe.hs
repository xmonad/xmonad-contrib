{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnNamedPipe
-- Copyright   :  (c) Christian Wills 2014 
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  cwills.dev@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a pipe whose handle lives in the Xmonad state. This
-- makes is possible to start dzen in the startup hook and pipe stuff to it in
-- the logHook cleanly. 
--
-----------------------------------------------------------------------------

module XMonad.Util.SpawnNamedPipe (spawnNamedPipe, getNamedPipeHandle) where

import XMonad
import XMonad.Util.Run
import System.IO
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad
import qualified Data.Map.Strict as Map

data NamedPipes = NamedPipes { pipeMap :: (Map.Map String Handle) }
    deriving (Show, Typeable)

instance ExtensionClass NamedPipes where
    initialValue = NamedPipes Map.empty 

spawnNamedPipe :: String -> String -> X ()
spawnNamedPipe cmd name = do
  b <- XS.gets (Map.member name . pipeMap) 
  when (not b) $ do
    h <- spawnPipe cmd 
    XS.modify (NamedPipes . Map.insert name h . pipeMap)   

getNamedPipeHandle :: String -> X (Maybe Handle)
getNamedPipeHandle name = XS.gets (Map.lookup name . pipeMap)
