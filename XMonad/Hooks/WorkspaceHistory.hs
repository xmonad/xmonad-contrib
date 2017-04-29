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

module XMonad.Hooks.WorkspaceHistory (
      -- * Usage
      -- $usage
      -- * Hooking
    workspaceHistoryHook
      -- * Querying
  , workspaceHistory
  , workspaceHistoryByScreen
  , workspaceHistoryWithScreen
    -- * Handling edits
  , workspaceHistoryTransaction
  ) where

import           Control.Applicative
import           Prelude

import XMonad
import XMonad.StackSet hiding (filter, delete)
import Data.List
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

data WorkspaceHistory = WorkspaceHistory
  { history :: [(ScreenId, WorkspaceId)] -- ^ Workspace Screens in
                                         -- reverse-chronological order.
  } deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceHistory where
    initialValue = WorkspaceHistory []
    extensionType = PersistentExtension

-- | A 'logHook' that keeps track of the order in which workspaces have
-- been viewed.
workspaceHistoryHook :: X ()
workspaceHistoryHook = gets windowset >>= (XS.modify . updateLastActiveOnEachScreen)

workspaceHistoryWithScreen :: X [(ScreenId, WorkspaceId)]
workspaceHistoryWithScreen = XS.gets history

workspaceHistoryByScreen :: X [(ScreenId, [WorkspaceId])]
workspaceHistoryByScreen =
  map (\wss -> (fst $ head wss, map snd wss)) .
  groupBy (\a b -> fst a == fst b) .
  sortBy (\a b -> compare (fst a) $ fst b)<$>
  workspaceHistoryWithScreen

-- | A list of workspace tags in the order they have been viewed, with the
-- most recent first. No duplicates are present, but not all workspaces are
-- guaranteed to appear, and there may be workspaces that no longer exist.
workspaceHistory :: X [WorkspaceId]
workspaceHistory = nub . map snd <$> XS.gets history

workspaceHistoryTransaction :: X () -> X ()
workspaceHistoryTransaction action = do
  startingHistory <- XS.gets history
  action
  new <- (flip updateLastActiveOnEachScreen $ WorkspaceHistory startingHistory) <$> gets windowset
  XS.put new

-- | Update the last visible workspace on each monitor if needed
-- already there, or move it to the front if it is.
updateLastActiveOnEachScreen :: WindowSet -> WorkspaceHistory -> WorkspaceHistory
updateLastActiveOnEachScreen StackSet {current = cur, visible = vis} wh =
  WorkspaceHistory { history = doUpdate cur $ foldl updateLastForScreen (history wh) $ vis ++ [cur] }
  where
    firstOnScreen sid = find ((== sid) . fst)
    doUpdate Screen {workspace = Workspace { tag = wid }, screen = sid} curr =
      let newEntry = (sid, wid) in newEntry:delete newEntry curr
    updateLastForScreen curr Screen {workspace = Workspace { tag = wid }, screen = sid} =
      let newEntry = (sid, wid)
          alreadyCurrent = maybe False (== newEntry) $ firstOnScreen sid curr
      in if alreadyCurrent then curr else newEntry:delete newEntry curr
