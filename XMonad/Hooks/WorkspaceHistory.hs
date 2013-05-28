{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceHistory
-- Copyright   :  (c) 2013 Dmitri Iouchtchenko
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Dmitri Iouchtchenko <johnnyspoon@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Keeps track of workspace viewing order.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.WorkspaceHistory
    ( -- * Usage
      -- $usage

      -- * Hooking
      workspaceHistoryHook

      -- * Querying
    , workspaceHistory

    ) where

import XMonad
import XMonad.StackSet (currentTag)
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- To record the order in which you view workspaces, you can use this
-- module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
--
-- Then add the hook to your 'logHook':
--
-- >  main = xmonad $ def
-- >      { ...
-- >      , logHook = ... >> workspaceHistoryHook >> ...
-- >      , ...
-- >      }
--
-- To make use of the collected data, a query function is provided.

data WorkspaceHistory =
    WorkspaceHistory { history :: [WorkspaceId] -- ^ Workspaces in reverse-chronological order.
                     }
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceHistory where
    initialValue = WorkspaceHistory []
    extensionType = PersistentExtension

-- | A 'logHook' that keeps track of the order in which workspaces have
-- been viewed.
workspaceHistoryHook :: X ()
workspaceHistoryHook = gets (currentTag . windowset) >>= (XS.modify . makeFirst)

-- | A list of workspace tags in the order they have been viewed, with the
-- most recent first. No duplicates are present, but not all workspaces are
-- guaranteed to appear, and there may be workspaces that no longer exist.
workspaceHistory :: X [WorkspaceId]
workspaceHistory = XS.gets history


-- | Cons the 'WorkspaceId' onto the 'WorkspaceHistory' if it is not
-- already there, or move it to the front if it is.
makeFirst :: WorkspaceId -> WorkspaceHistory -> WorkspaceHistory
makeFirst w v = let (xs, ys) = break (w ==) $ history v
                in v { history = w : (xs ++ drop 1 ys) }
