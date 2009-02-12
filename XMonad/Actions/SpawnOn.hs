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
-- Provides a way to spawn an application on a specific workspace by using
-- the _NET_WM_PID property that most windows set on creation. Hence this module
-- won't work on applications that don't set this property.
--
-----------------------------------------------------------------------------

module XMonad.Actions.SpawnOn (
    -- * Usage
    -- $usage
    Spawner,
    mkSpawner,
    manageSpawn,
    spawnHere,
    spawnOn,
    shellPromptHere,
    shellPromptOn
) where

import Data.List (isInfixOf)
import Data.IORef
import System.Posix.Types (ProcessID)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.SpawnOn
--
-- >    main = do
-- >      sp <- mkSpawner
-- >      xmonad defaultConfig {
-- >         ...
-- >         manageHook = manageSpawn sp <+> manageHook defaultConfig
-- >         ...
-- >      }
--
-- To ensure that application appears on a workspace it was launched at, add keybindings like:
--
-- >  , ((mod1Mask,xK_o), spawnHere sp "urxvt")
-- >  , ((mod1Mask,xK_s), shellPromptHere sp defaultXPConfig)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype Spawner = Spawner {pidsRef :: IORef [(ProcessID, WorkspaceId)]}

maxPids :: Int
maxPids = 5

-- | Create 'Spawner' which then has to be passed to other functions.
mkSpawner :: (Functor m, MonadIO m) => m Spawner
mkSpawner = io . fmap Spawner $ newIORef []

-- | Provides a manage hook to react on process spawned with
-- 'spawnOn', 'spawnHere' etc.
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

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on current workspace.
shellPromptHere :: Spawner -> XPConfig -> X ()
shellPromptHere sp = mkPrompt (spawnHere sp)

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on given workspace.
shellPromptOn :: Spawner -> WorkspaceId -> XPConfig -> X ()
shellPromptOn sp ws = mkPrompt (spawnOn sp ws)

-- | Replacement for 'spawn' which launches
-- application on current workspace.
spawnHere :: Spawner -> String -> X ()
spawnHere sp cmd = withWindowSet $ \ws -> spawnOn sp (W.currentTag ws) cmd

-- | Replacement for 'spawn' which launches
-- application on given workspace.
spawnOn :: Spawner -> WorkspaceId -> String -> X ()
spawnOn sp ws cmd = do
    p <- spawnPID $ mangle cmd
    io $ modifyIORef (pidsRef sp) (take maxPids . ((p, ws) :))
 where
    -- TODO this is silly, search for a better solution
    mangle xs | any (`elem` metaChars) xs || "exec" `isInfixOf` xs = xs
              | otherwise = "exec " ++ xs
    metaChars = "&|;"

