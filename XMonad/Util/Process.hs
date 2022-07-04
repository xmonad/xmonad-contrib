{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  XMonad.Util.Process
-- Description :  Utilities for unix processes.
-- Copyright   :  (c) 2022 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- This module should not be directly used by users, it's just common code for
-- other modules.
--
module XMonad.Util.Process (
    getPPIDOf,
    getPPIDChain,
    ) where

import Control.Exception (SomeException, handle)
import System.Posix.Types (ProcessID)
import qualified Data.ByteString.Char8 as B

import XMonad.Prelude (fi)

-- | Get the parent process id (PPID) of a given process.
getPPIDOf :: ProcessID -> IO (Maybe ProcessID)
getPPIDOf pid =
    handle
        (\(_ :: SomeException) -> pure Nothing)
        (parse <$> B.readFile ("/proc/" <> show pid <> "/stat"))
  where
    -- Parse PPID out of /proc/*/stat, being careful not to trip over
    -- processes with names like ":-) 1 2 3 4 5 6".
    -- Inspired by https://gitlab.com/procps-ng/procps/-/blob/bcce3e440a1e1ee130c7371251a39c031519336a/proc/readproc.c#L561
    parse stat = case B.words $ snd $ B.spanEnd (/= ')') stat of
        _ : (B.readInt -> Just (ppid, ""))  : _ -> Just (fi ppid)
        _ -> Nothing

-- | Get the chain of parent processes of a given pid. Starts with the given
-- pid and continues up until the parent of all.
getPPIDChain :: ProcessID -> IO [ProcessID]
getPPIDChain pid = (pid :) <$> (maybe (pure []) getPPIDChain =<< getPPIDOf pid)
