{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnOnce
-- Copyright   :  (c) Spencer Janssen 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a command once, and only once.  Useful to start
-- status bars and make session settings inside startupHook.
--
-----------------------------------------------------------------------------

module XMonad.Util.SpawnOnce (spawnOnce, spawnOnOnce, spawnNOnOnce, spawnAndDoOnce) where

import XMonad
import XMonad.Actions.SpawnOn
import Data.Set as Set
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad

data SpawnOnce = SpawnOnce { unspawnOnce :: (Set String) }
    deriving (Read, Show, Typeable)

instance ExtensionClass SpawnOnce where
    initialValue = SpawnOnce Set.empty
    extensionType = PersistentExtension

doOnce :: (String -> X ()) -> String -> X ()
doOnce f s = do
    b <- XS.gets (Set.member s . unspawnOnce)
    when (not b) $ do
        f s
        XS.modify (SpawnOnce . Set.insert s . unspawnOnce)


-- | The first time 'spawnOnce' is executed on a particular command,
-- that command is executed.  Subsequent invocations for a command do
-- nothing.
spawnOnce :: String -> X ()
spawnOnce cmd = doOnce spawn cmd

-- | Like spawnOnce but launches the application on the given workspace.
spawnOnOnce :: WorkspaceId -> String -> X ()
spawnOnOnce ws cmd = doOnce (spawnOn ws) cmd

-- | Lanch the given application n times on the specified
-- workspace. Subsequent attempts to spawn this application will be
-- ignored.
spawnNOnOnce :: Int -> WorkspaceId -> String -> X ()
spawnNOnOnce n ws cmd = doOnce (\c -> sequence_ $ replicate n $ spawnOn ws c) cmd

-- | Spawn the application once and apply the manage hook. Subsequent
-- attempts to spawn this application will be ignored.
spawnAndDoOnce :: ManageHook -> String -> X ()
spawnAndDoOnce mh cmd = doOnce (spawnAndDo mh) cmd
