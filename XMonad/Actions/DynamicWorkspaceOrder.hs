-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicWorkspaceOrder
-- Description :  Remember a dynamically updateable ordering on workspaces.
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-- Remember a dynamically updateable ordering on workspaces, together
-- with tools for using this ordering with "XMonad.Actions.CycleWS"
-- and "XMonad.Hooks.StatusBar.PP".
--
-----------------------------------------------------------------------------

module XMonad.Actions.DynamicWorkspaceOrder
    ( -- * Usage
      -- $usage

      getWsCompareByOrder
    , getSortByOrder
    , swapWith
    , swapWithCurrent
    , swapOrder
    , updateName
    , removeName

    , moveTo
    , moveToGreedy
    , shiftTo

    , withNthWorkspace'
    , withNthWorkspace

    ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.WorkspaceCompare (WorkspaceCompare, WorkspaceSort, mkWsSort)
import XMonad.Actions.CycleWS (findWorkspace, WSType(..), Direction1D(..), doTo)

import qualified Data.Map as M
import qualified Data.Set as S
import XMonad.Prelude (fromJust, fromMaybe)
import Data.Ord (comparing)

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
--
-- Then add keybindings to swap the order of workspaces (these
-- examples use "XMonad.Util.EZConfig" emacs-style keybindings):
--
-- >        , ("M-C-<R>",   DO.swapWith Next NonEmptyWS)
-- >        , ("M-C-<L>",   DO.swapWith Prev NonEmptyWS)
--
-- See "XMonad.Actions.CycleWS" for information on the possible
-- arguments to 'swapWith'.
--
-- However, by itself this will do nothing; 'swapWith' does not change
-- the actual workspaces in any way.  It simply keeps track of an
-- auxiliary ordering on workspaces.  Anything which cares about the
-- order of workspaces must be updated to use the auxiliary ordering.
--
-- To change the order in which workspaces are displayed by
-- "XMonad.Hooks.StatusBar.PP", use 'getSortByOrder' in your
-- 'XMonad.Hooks.StatusBar.PP.ppSort' field, for example:
--
-- >   myPP = ... byorgeyPP {
-- >     ...
-- >     , ppSort = DO.getSortByOrder
-- >     ...
-- >   }
--
-- To use workspace cycling commands like those from
-- "XMonad.Actions.CycleWS", use the versions of 'moveTo',
-- 'moveToGreedy', and 'shiftTo' exported by this module.  For example:
--
-- >     , ("M-S-<R>",   DO.shiftTo Next HiddenNonEmptyWS)
-- >     , ("M-S-<L>",   DO.shiftTo Prev HiddenNonEmptyWS)
-- >     , ("M-<R>",     DO.moveTo Next HiddenNonEmptyWS)
-- >     , ("M-<L>",     DO.moveTo Prev HiddenNonEmptyWS)
--
-- For slight variations on these, use the source for examples and
-- tweak as desired.

-- | Extensible state storage for the workspace order.
newtype WSOrderStorage = WSO { unWSO :: Maybe (M.Map WorkspaceId Int) }
  deriving (Read, Show)

instance ExtensionClass WSOrderStorage where
  initialValue = WSO Nothing
  extensionType = PersistentExtension

-- | Lift a Map function to a function on WSOrderStorage.
withWSO :: (M.Map WorkspaceId Int -> M.Map WorkspaceId Int)
           -> (WSOrderStorage -> WSOrderStorage)
withWSO f = WSO . fmap f . unWSO

-- | Update the ordering storage: initialize if it doesn't yet exist;
-- add newly created workspaces at the end as necessary.
updateOrder :: X ()
updateOrder = do
  WSO mm <- XS.get
  case mm of
    Nothing -> do
      -- initialize using ordering of workspaces from the user's config
      ws <- asks (workspaces . config)
      XS.put . WSO . Just . M.fromList $ zip ws [0..]
    Just m -> do
      -- check for new workspaces and add them at the end
      curWs <- gets (S.fromList . map W.tag . W.workspaces . windowset)
      let mappedWs  = M.keysSet m
          newWs     = curWs `S.difference` mappedWs
          nextIndex = 1 + maximum (-1 : M.elems m)
          newWsIxs  = zip (S.toAscList newWs) [nextIndex..]
      XS.modify . withWSO . M.union . M.fromList $ newWsIxs

-- | A comparison function which orders workspaces according to the
-- stored dynamic ordering.
getWsCompareByOrder :: X WorkspaceCompare
getWsCompareByOrder = do
  updateOrder
  -- after the call to updateOrder we are guaranteed that the dynamic
  -- workspace order is initialized and contains all existing
  -- workspaces.
  WSO (Just m) <- XS.get
  return $ comparing (fromMaybe 1000 . flip M.lookup m)

-- | Sort workspaces according to the stored dynamic ordering.
getSortByOrder :: X WorkspaceSort
getSortByOrder = mkWsSort getWsCompareByOrder

-- | Swap the current workspace with another workspace in the stored
-- dynamic order.
swapWith :: Direction1D -> WSType -> X ()
swapWith dir which = findWorkspace getSortByOrder dir which 1 >>= swapWithCurrent

-- | Swap the given workspace with the current one.
swapWithCurrent :: WorkspaceId -> X ()
swapWithCurrent w = do
  cur <- gets (W.currentTag . windowset)
  swapOrder w cur

-- | Swap the two given workspaces in the dynamic order.
swapOrder :: WorkspaceId -> WorkspaceId -> X ()
swapOrder w1 w2 = do
  io $ print (w1,w2)
  WSO (Just m) <- XS.get
  let i1 = fromJust (w1 `M.lookup` m)
  let i2 = fromJust (w2 `M.lookup` m)
  XS.modify (withWSO (M.insert w1 i2 . M.insert w2 i1))
  windows id  -- force a status bar update

-- | Update the name of a workspace in the stored order.
updateName :: WorkspaceId -> WorkspaceId -> X ()
updateName oldId newId = XS.modify . withWSO $ changeKey oldId newId

-- | Remove a workspace from the stored order.
removeName :: WorkspaceId -> X ()
removeName = XS.modify . withWSO . M.delete

-- | Update a key in a Map.
changeKey :: Ord k => k -> k -> M.Map k a -> M.Map k a
changeKey oldKey newKey oldMap =
  case M.updateLookupWithKey (\_ _ -> Nothing) oldKey oldMap of
    (Nothing, _) -> oldMap
    (Just val, newMap) -> M.insert newKey val newMap

-- | View the next workspace of the given type in the given direction,
-- where \"next\" is determined using the dynamic workspace order.
moveTo :: Direction1D -> WSType -> X ()
moveTo dir t = doTo dir t getSortByOrder (windows . W.view)

-- | Same as 'moveTo', but using 'greedyView' instead of 'view'.
moveToGreedy :: Direction1D -> WSType -> X ()
moveToGreedy dir t = doTo dir t getSortByOrder (windows . W.greedyView)

-- | Shift the currently focused window to the next workspace of the
-- given type in the given direction, using the dynamic workspace order.
shiftTo :: Direction1D -> WSType -> X ()
shiftTo dir t = doTo dir t getSortByOrder (windows . W.shift)

-- | Do something with the nth workspace in the dynamic order after
--   transforming it.  The callback is given the workspace's tag as well
--   as the 'WindowSet' of the workspace itself.
withNthWorkspace' :: ([WorkspaceId] -> [WorkspaceId]) -> (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace' tr job wnum = do
  sort <- getSortByOrder
  ws <- gets (tr . map W.tag . sort . W.workspaces . windowset)
  case drop wnum ws of
    (w:_) -> windows $ job w
    []    -> return ()

-- | Do something with the nth workspace in the dynamic order.  The
--   callback is given the workspace's tag as well as the 'WindowSet'
--   of the workspace itself.
withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace = withNthWorkspace' id
