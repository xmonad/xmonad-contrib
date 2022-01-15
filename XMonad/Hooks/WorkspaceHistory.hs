-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceHistory
-- Description :  Keep track of workspace viewing order.
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
  , workspaceHistoryHookExclude
      -- * Querying
  , workspaceHistory
  , workspaceHistoryByScreen
  , workspaceHistoryWithScreen
    -- * Handling edits
  , workspaceHistoryTransaction
  , workspaceHistoryModify
  ) where

import Control.Applicative
import Control.DeepSeq
import Prelude
import XMonad
import XMonad.StackSet hiding (delete, filter, new)
import XMonad.Prelude (delete, find, foldl', groupBy, nub, sortBy)
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
-- If you want to completely exclude certain workspaces from entering
-- the history, you can use 'workspaceHistoryHookExclude' instead.  For
-- example, to ignore the named scratchpad workspace:
--
-- > import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
-- > ...
-- > , logHook = ... >> workspaceHistoryHookExclude [scratchpadWorkspaceTag] >> ...
--
-- To make use of the collected data, a query function is provided.

newtype WorkspaceHistory = WorkspaceHistory
  { history :: [(ScreenId, WorkspaceId)] -- ^ Workspace Screens in
                                         -- reverse-chronological order.
  } deriving (Read, Show)

-- @ScreenId@ is not an instance of NFData, but is a newtype on @Int@. @seq@
-- is enough for forcing it. This requires us to provide an instance.
instance NFData WorkspaceHistory where
  rnf (WorkspaceHistory hist) =
    let go = liftRnf2 rwhnf rwhnf
    in liftRnf go hist

instance ExtensionClass WorkspaceHistory where
    initialValue = WorkspaceHistory []
    extensionType = PersistentExtension

-- | A 'logHook' that keeps track of the order in which workspaces have
-- been viewed.
workspaceHistoryHook :: X ()
workspaceHistoryHook = workspaceHistoryHookExclude []

-- | Like 'workspaceHistoryHook', but with the ability to exclude
-- certain workspaces.
workspaceHistoryHookExclude :: [WorkspaceId] -> X ()
workspaceHistoryHookExclude ws = XS.modify' . update =<< gets windowset
  where
    update :: WindowSet -> WorkspaceHistory -> WorkspaceHistory
    update s = force . updateLastActiveOnEachScreenExclude ws s

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
  new <- flip updateLastActiveOnEachScreen (WorkspaceHistory startingHistory) <$> gets windowset
  XS.put $! force new

-- | Update the last visible workspace on each monitor if needed
-- already there, or move it to the front if it is.
updateLastActiveOnEachScreen :: WindowSet -> WorkspaceHistory -> WorkspaceHistory
updateLastActiveOnEachScreen = updateLastActiveOnEachScreenExclude []

-- | Like 'updateLastActiveOnEachScreen', but with the ability to
-- exclude certain workspaces.
updateLastActiveOnEachScreenExclude :: [WorkspaceId] -> WindowSet -> WorkspaceHistory -> WorkspaceHistory
updateLastActiveOnEachScreenExclude ws StackSet {current = cur, visible = vis} wh =
  WorkspaceHistory { history = doUpdate cur $ foldl' updateLastForScreen (history wh) $ vis ++ [cur] }
  where
    firstOnScreen sid = find ((== sid) . fst)
    doUpdate Screen {workspace = Workspace { tag = wid }, screen = sid} curr =
      let newEntry = (sid, wid)
       in if wid `elem` ws then curr else newEntry : delete newEntry curr
    updateLastForScreen curr Screen {workspace = Workspace { tag = wid }, screen = sid} =
      let newEntry = (sid, wid)
          alreadyCurrent = Just newEntry == firstOnScreen sid curr
      in if alreadyCurrent || wid `elem` ws then curr else newEntry : delete newEntry curr

-- | Modify a the workspace history with a given pure function.
workspaceHistoryModify :: ([(ScreenId, WorkspaceId)] -> [(ScreenId, WorkspaceId)]) -> X ()
workspaceHistoryModify action = XS.modify' $ force . WorkspaceHistory . action . history
