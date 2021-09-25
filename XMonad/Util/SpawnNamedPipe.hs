-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnNamedPipe
-- Description :  A module for spawning a pipe whose handle lives in the XMonad state.
-- Copyright   :  (c) Christian Wills 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  cwills.dev@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a pipe whose "Handle" lives in the Xmonad state.
--
-----------------------------------------------------------------------------

module XMonad.Util.SpawnNamedPipe (
  -- * Usage
  -- $usage
    spawnNamedPipe
  , getNamedPipe
  ) where

import XMonad
import XMonad.Util.Run
import System.IO
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prelude
import qualified Data.Map as Map

-- $usage
-- This module makes it possible to spawn a pipe to Dzen2 in the startupHook
-- and write to it from inside the logHook without the need for global
-- variables.
--
-- > import XMonad.Util.SpawnNamedPipe
-- > import Data.Maybe
-- >
-- > -- StartupHook
-- > startupHook' = spawnNamedPipe "dzen2" "dzenPipe"
-- >
-- > -- LogHook
-- > logHook' = do
-- >     mh <- getNamedPipeHandle "dzenPipe"
-- >         dynamicLogWithPP $ def {
-- >             ppOutput = maybe (\s -> return ()) (hPutStrLn) mh}
-- >
-- > -- Main
-- > main = xmonad $ def { startupHook = startupHook'
-- >                     , logHook = logHook'}
--

newtype NamedPipes = NamedPipes { pipeMap :: Map.Map String Handle }
    deriving (Show)

instance ExtensionClass NamedPipes where
    initialValue = NamedPipes Map.empty

-- | When 'spawnNamedPipe' is executed with a command "String" and a name
-- "String" respectively.  The command string is spawned with 'spawnPipe' (as
-- long as the name chosen hasn't been used already) and the "Handle" returned
-- is saved in Xmonad's state associated with the name "String".
spawnNamedPipe :: String -> String -> X ()
spawnNamedPipe cmd name = do
  b <- XS.gets (Map.member name . pipeMap)
  unless b $ do
    h <- spawnPipe cmd
    XS.modify (NamedPipes . Map.insert name h . pipeMap)

-- | Attempts to retrieve a "Handle" to a pipe previously stored in Xmonad's
-- state associated with the given string via a call to 'spawnNamedPipe'. If the
-- given string doesn't exist in the map stored in Xmonad's state Nothing is
-- returned.
getNamedPipe :: String -> X (Maybe Handle)
getNamedPipe name = XS.gets (Map.lookup name . pipeMap)
