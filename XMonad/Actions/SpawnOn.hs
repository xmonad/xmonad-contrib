-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.SpawnOn
-- Copyright    : (c) Spencer Janssen
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides helper functions to be used in @manageHook@. Here's
-- how you might use this:
--
-- > import XMonad.Hooks.ManageHelpers
-- > main = do
-- >     sp <- mkSpawner
-- >     xmonad defaultConfig {
-- >         ...
-- >         manageHook = spawnHook sp <+> manageHook defaultConfig
-- >         ...
-- >     }

module XMonad.Actions.SpawnOn (
    Spawner,
    mkSpawner,
    manageSpawn,
    spawnHere,
    spawnOn,
    shellPromptHere,
    shellPromptOn
) where

import Data.IORef
import System.Posix.Types (ProcessID)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell

newtype Spawner = Spawner {pidsRef :: IORef [(ProcessID, WorkspaceId)]}

maxPids :: Int
maxPids = 5

mkSpawner :: (Functor m, MonadIO m) => m Spawner
mkSpawner = io . fmap Spawner $ newIORef []

manageSpawn :: Spawner -> ManageHook
manageSpawn sp = do
    pids <- io . readIORef $ pidsRef sp
    mp <- pid
    case flip lookup pids =<< mp of
        Just w -> doF (W.shift w)
        Nothing -> doF id

mkPrompt :: (String -> X ()) -> XPConfig -> X ()
mkPrompt cb c = do
    cmds <- io $ getCommands
    mkXPrompt Shell c (getShellCompl cmds) cb

shellPromptHere :: Spawner -> XPConfig -> X ()
shellPromptHere sp = mkPrompt (spawnHere sp)

shellPromptOn :: Spawner -> WorkspaceId -> XPConfig -> X ()
shellPromptOn sp ws = mkPrompt (spawnOn sp ws)

spawnHere :: Spawner -> String -> X ()
spawnHere sp cmd = withWindowSet $ \ws -> spawnOn sp (W.currentTag ws) cmd

spawnOn :: Spawner -> WorkspaceId -> String -> X ()
spawnOn sp ws cmd = do
    p <- spawnPID cmd
    io $ modifyIORef (pidsRef sp) (take maxPids . ((p, ws) :))
