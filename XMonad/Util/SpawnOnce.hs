-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnOnce
-- Description :  A module for spawning a command once, and only once.
-- Copyright   :  (c) Spencer Janssen 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a command once, and only once.  Useful to start
-- status bars and make session settings inside startupHook. See also
-- 'XMonad.Util.SessionStart' for a different and more flexible way to
-- run commands only on first startup.
--
-----------------------------------------------------------------------------

module XMonad.Util.SpawnOnce (spawnOnce,
                              -- * 'SpawnOn' helpers
                              -- $spawnon
                              manageSpawn,
                              spawnOnOnce,
                              spawnNOnOnce,
                              spawnAndDoOnce) where

import XMonad
import XMonad.Actions.SpawnOn
import Data.Set as Set
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prelude

newtype SpawnOnce = SpawnOnce { unspawnOnce :: Set String }
    deriving (Read, Show)

instance ExtensionClass SpawnOnce where
    initialValue = SpawnOnce Set.empty
    extensionType = PersistentExtension

doOnce :: (String -> X ()) -> String -> X ()
doOnce f s = do
    b <- XS.gets (Set.member s . unspawnOnce)
    unless b $ do
        f s
        XS.modify (SpawnOnce . Set.insert s . unspawnOnce)


-- | The first time 'spawnOnce' is executed on a particular command,
-- that command is executed.  Subsequent invocations for a command do
-- nothing.
spawnOnce :: String -> X ()
spawnOnce = doOnce spawn

-- $spawnon
-- These functions combine 'spawnOnce' with their relatives in
-- 'XMonad.Actions.SpawnOn'. You must add 'manageSpawn' to your
-- @manageHook@ for them to work, as with @SpawnOn@.

-- | Like 'spawnOnce' but launches the application on the given workspace.
spawnOnOnce :: WorkspaceId -> String -> X ()
spawnOnOnce ws = doOnce (spawnOn ws)

-- | Lanch the given application n times on the specified
-- workspace. Subsequent attempts to spawn this application will be
-- ignored.
spawnNOnOnce :: Int -> WorkspaceId -> String -> X ()
spawnNOnOnce n ws = doOnce (replicateM_ n . spawnOn ws)

-- | Spawn the application once and apply the manage hook. Subsequent
-- attempts to spawn this application will be ignored.
spawnAndDoOnce :: ManageHook -> String -> X ()
spawnAndDoOnce mh = doOnce (spawnAndDo mh)
