{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.SpawnOn
-- Description  : Modify a window spawned by a command.
-- Copyright    : (c) Spencer Janssen
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides a way to modify a window spawned by a command(e.g shift it to the workspace
-- it was launched on) by using the _NET_WM_PID property that most windows set on creation.
-- Hence this module won't work on applications that don't set this property.
--
-----------------------------------------------------------------------------

module XMonad.Actions.SpawnOn (
    -- * Usage
    -- $usage
    Spawner,
    manageSpawn,
    manageSpawnWithGC,
    spawnHere,
    spawnOn,
    spawnAndDo,
    shellPromptHere,
    shellPromptOn
) where

import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Process (getPPIDChain)

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- >    import XMonad.Actions.SpawnOn
--
-- >    main = do
-- >      xmonad def {
-- >         ...
-- >         manageHook = manageSpawn <> manageHook def
-- >         ...
-- >      }
--
-- To ensure that application appears on a workspace it was launched at, add keybindings like:
--
-- >  , ((mod1Mask,xK_o), spawnHere "urxvt")
-- >  , ((mod1Mask,xK_s), shellPromptHere def)
--
-- The module can also be used to apply other manage hooks to the window of
-- the spawned application(e.g. float or resize it).
--
-- For detailed instructions on editing your key bindings, see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.

newtype Spawner = Spawner {pidsRef :: [(ProcessID, ManageHook)]}

instance ExtensionClass Spawner where
    initialValue = Spawner []


-- | Get the current Spawner or create one if it doesn't exist.
modifySpawner :: ([(ProcessID, ManageHook)] -> [(ProcessID, ManageHook)]) -> X ()
modifySpawner f = XS.modify (Spawner . f . pidsRef)

modifySpawnerM :: ([(ProcessID, ManageHook)] -> X [(ProcessID, ManageHook)]) -> X ()
modifySpawnerM f = XS.modifyM (fmap Spawner . f . pidsRef)

-- | Provides a manage hook to react on process spawned with
-- 'spawnOn', 'spawnHere' etc.
manageSpawn :: ManageHook
manageSpawn = manageSpawnWithGC (return . take 20)

manageSpawnWithGC :: ([(ProcessID, ManageHook)] -> X [(ProcessID, ManageHook)])
        -- ^ function to stop accumulation of entries for windows that never set @_NET_WM_PID@
       -> ManageHook
manageSpawnWithGC garbageCollect = pid >>= \case
    Nothing -> mempty
    Just p -> do
        Spawner pids <- liftX XS.get
        ppid_chain <- io $ getPPIDChain p
        case mapMaybe (`lookup` pids) ppid_chain of
            [] -> mempty
            mh : _ -> liftX (gc p) >> mh
  where
    gc p = modifySpawnerM $ garbageCollect . filter ((/= p) . fst)

mkPrompt :: (String -> X ()) -> XPConfig -> X ()
mkPrompt cb c = do
    cmds <- io getCommands
    mkXPrompt Shell c (getShellCompl cmds $ searchPredicate c) cb

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on current workspace.
shellPromptHere :: XPConfig -> X ()
shellPromptHere = mkPrompt spawnHere

-- | Replacement for Shell prompt ("XMonad.Prompt.Shell") which launches
-- application on given workspace.
shellPromptOn :: WorkspaceId -> XPConfig -> X ()
shellPromptOn ws = mkPrompt (spawnOn ws)

-- | Replacement for 'spawn' which launches
-- application on current workspace.
spawnHere :: String -> X ()
spawnHere cmd = withWindowSet $ \ws -> spawnOn (W.currentTag ws) cmd

-- | Replacement for 'spawn' which launches
-- application on given workspace.
spawnOn :: WorkspaceId -> String -> X ()
spawnOn ws = spawnAndDo (doShift ws)

-- | Spawn an application and apply the manage hook when it opens.
spawnAndDo :: ManageHook -> String -> X ()
spawnAndDo mh cmd = do
    p <- spawnPID $ mangle cmd
    modifySpawner ((p,mh) :)
 where
    -- TODO this is silly, search for a better solution
    mangle xs | any (`elem` metaChars) xs || "exec" `isInfixOf` xs = xs
              | otherwise = "exec " ++ xs
    metaChars = "&|;"
