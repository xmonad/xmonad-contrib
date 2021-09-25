-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SessionStart
-- Description :  A module for detectiong session startup.
-- Copyright   :  (c) Markus Ongyerth 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  markus@ongy.net
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for detectiong session startup.  Useful to start
-- status bars, compositors and session initialization.
-- This is a more general approach than spawnOnce and allows spawnOn etc.
-----------------------------------------------------------------------------

module XMonad.Util.SessionStart
    ( doOnce
    , isSessionStart
    , setSessionStarted
    )
where

import XMonad.Prelude (when)

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

-- ---------------------------------------------------------------------
-- $usage
--
-- Add 'setSessionStarted' at the end of the 'startupHook' to set the
-- flag.
--
-- To do something only when the session is started up, use
-- 'isSessionStart' to query or wrap it in 'doOnce' to only do it when
-- the flag isn't set.
-- ---------------------------------------------------------------------

newtype SessionStart = SessionStart { unSessionStart :: Bool }
    deriving (Read, Show)

instance ExtensionClass SessionStart where
    initialValue = SessionStart True
    extensionType = PersistentExtension

-- | Use this to only do a part of your hook on session start
doOnce :: X () -> X ()
doOnce act = do
    startup <- isSessionStart
    when startup act

-- | Query if the current startup is the session start
isSessionStart :: X Bool
isSessionStart = unSessionStart <$> XS.get

-- This should become a noop/be deprecated when merged into master, and
-- the flag should be set when the state file is loaded.
-- | This currently has to be added to the end of the startup hook to
-- set the flag.
setSessionStarted :: X ()
setSessionStarted = XS.put $ SessionStart False
