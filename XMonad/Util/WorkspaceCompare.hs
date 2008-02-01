-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WorkspaceCompare
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--

module XMonad.Util.WorkspaceCompare ( WorkspaceCompare, WorkspaceSort
                                    , getWsIndex
                                    , getWsCompare
                                    , getWsCompareByTag
                                    , getXineramaWsCompare
                                    , mkWsSort
                                    , getSortByIndex
                                    , getSortByTag
                                    , getSortByXineramaRule ) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import Data.Monoid
import Data.Ord
import Data.Maybe

type WorkspaceCompare = WorkspaceId -> WorkspaceId -> Ordering
type WorkspaceSort = [WindowSpace] -> [WindowSpace]

-- | Lookup the index of a workspace id in the user's config, return Nothing
-- if that workspace does not exist in the config.
getWsIndex :: X (WorkspaceId -> Maybe Int)
getWsIndex = do
    spaces <- asks (workspaces . config)
    return $ flip elemIndex spaces

-- | A comparison function for WorkspaceId, based on the index of the
--   tags in the user's config.
getWsCompare :: X WorkspaceCompare
getWsCompare = do
    wsIndex <- getWsIndex
    return $ \a b -> f (wsIndex a) (wsIndex b) `mappend` compare a b
  where
    f Nothing Nothing   = EQ
    f (Just _) Nothing  = LT
    f Nothing (Just _)  = GT
    f (Just x) (Just y) = compare x y

-- | A simple comparison function that orders workspaces
--   lexicographically by tag.
getWsCompareByTag :: X WorkspaceCompare
getWsCompareByTag = return compare

-- | A comparison function for Xinerama based on visibility, workspace
--   and screen id. It produces the same ordering as
--   'XMonad.Hooks.DynamicLog.pprWindowSetXinerama'.
getXineramaWsCompare :: X WorkspaceCompare
getXineramaWsCompare = do
    w <- gets windowset
    return $ \ a b -> case (isOnScreen a w, isOnScreen b w) of
        (True, True)   -> comparing (tagToSid (onScreen w)) a b
        (False, False) -> compare a b
        (True, False)  -> LT
        (False, True)  -> GT
  where
    onScreen w =  S.current w : S.visible w
    isOnScreen a w  = a `elem` map (S.tag . S.workspace) (onScreen w)
    tagToSid s x = S.screen $ fromJust $ find ((== x) . S.tag . S.workspace) s

-- | Create a workspace sorting function from a workspace comparison
--   function.
mkWsSort :: X WorkspaceCompare -> X WorkspaceSort
mkWsSort cmpX = do
  cmp <- cmpX
  return $ sortBy (\a b -> cmp (S.tag a) (S.tag b))

-- | Sort several workspaces according to their tags' indices in the
--   user's config.
getSortByIndex :: X WorkspaceSort
getSortByIndex = mkWsSort getWsCompare

-- | Sort workspaces lexicographically by tag.
getSortByTag :: X WorkspaceSort
getSortByTag = mkWsSort getWsCompareByTag

-- | Sort serveral workspaces for xinerama displays, in the same order
--   produced by 'XMonad.Hooks.DynamicLog.pprWindowSetXinerama': first
--   visible workspaces, sorted by screen, then hidden workspaces,
--   sorted by tag.
getSortByXineramaRule :: X WorkspaceSort
getSortByXineramaRule = mkWsSort getXineramaWsCompare

