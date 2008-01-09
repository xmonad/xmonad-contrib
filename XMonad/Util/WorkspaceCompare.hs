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

module XMonad.Util.WorkspaceCompare ( getWsIndex
                                    , getWsCompare
                                    , getSortByTag
                                    , getSortByXineramaRule ) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import Data.Monoid
import Data.Ord
import Data.Maybe

-- | Lookup the index of a workspace id in the user's config, return Nothing
-- if that workspace does not exist in the config.
getWsIndex :: X (WorkspaceId -> Maybe Int)
getWsIndex = do
    spaces <- asks (workspaces . config)
    return $ flip elemIndex spaces

-- | A comparison function for WorkspaceId
getWsCompare :: X (WorkspaceId -> WorkspaceId -> Ordering)
getWsCompare = do
    wsIndex <- getWsIndex
    return $ \a b -> f (wsIndex a) (wsIndex b) `mappend` compare a b
  where
    f Nothing Nothing   = EQ
    f (Just _) Nothing  = LT
    f Nothing (Just _)  = GT
    f (Just x) (Just y) = compare x y

-- | A comparison function for Xinerama based on visibility, workspace and
-- screen id. It produces same ordering as pprWindowSetXinerama does.
getXineramaWsCompare :: X(WorkspaceId -> WorkspaceId -> Ordering)
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
                 --S.screen $ head $ filter ((== x) . S.tag . S.workspace) s

-- | Sort several workspaces according to the order in getWsCompare
getSortByTag :: X ([WindowSpace] -> [WindowSpace])
getSortByTag = do
    cmp <- getWsCompare
    return $ sortBy (\a b -> cmp (S.tag a) (S.tag b))

-- | Sort serveral workspaces for xinerama displays
getSortByXineramaRule :: X ([WindowSpace] -> [WindowSpace])
getSortByXineramaRule = do
    cmp <- getXineramaWsCompare
    return $ sortBy (\a b -> cmp (S.tag a) (S.tag b))

