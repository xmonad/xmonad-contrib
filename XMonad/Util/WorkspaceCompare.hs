-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WorkspaceCompare
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Util.WorkspaceCompare ( WorkspaceCompare, WorkspaceSort
                                    , getWsIndex
                                    , getWsCompare
                                    , getWsCompareByTag
                                    , getXineramaPhysicalWsCompare
                                    , getXineramaWsCompare
                                    , mkWsSort
                                    , getSortByIndex
                                    , getSortByTag
                                    , getSortByXineramaPhysicalRule
                                    , getSortByXineramaRule ) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import Data.Monoid
import Data.Ord
import Data.Maybe
import Data.Function

type WorkspaceCompare = WorkspaceId -> WorkspaceId -> Ordering
type WorkspaceSort = [WindowSpace] -> [WindowSpace]

-- | Lookup the index of a workspace id in the user's config, return Nothing
-- if that workspace does not exist in the config.
getWsIndex :: X (WorkspaceId -> Maybe Int)
getWsIndex = do
    spaces <- asks (workspaces . config)
    return $ flip elemIndex spaces

-- | Compare Maybe's differently, so Nothing (i.e. workspaces without indexes)
-- come last in the order
indexCompare :: Maybe Int -> Maybe Int -> Ordering
indexCompare Nothing Nothing = EQ
indexCompare Nothing (Just _) = GT
indexCompare (Just _) Nothing = LT
indexCompare a b = compare a b

-- | A comparison function for WorkspaceId, based on the index of the
--   tags in the user's config.
getWsCompare :: X WorkspaceCompare
getWsCompare = do
    wsIndex <- getWsIndex
    return $ mconcat [indexCompare `on` wsIndex, compare]

-- | A simple comparison function that orders workspaces
--   lexicographically by tag.
getWsCompareByTag :: X WorkspaceCompare
getWsCompareByTag = return compare

-- | A comparison function for Xinerama based on visibility, workspace
--   and screen id. It produces the same ordering as
--   'XMonad.Hooks.DynamicLog.pprWindowSetXinerama'.
getXineramaWsCompare :: X WorkspaceCompare
getXineramaWsCompare = getXineramaWsCompare' False

-- | A comparison function like 'getXineramaWsCompare', but uses physical locations for screens.
getXineramaPhysicalWsCompare :: X WorkspaceCompare
getXineramaPhysicalWsCompare = getXineramaWsCompare' True

getXineramaWsCompare' :: Bool -> X WorkspaceCompare
getXineramaWsCompare' phy = do
    w <- gets windowset
    return $ \ a b -> case (isOnScreen a w, isOnScreen b w) of
        (True, True)   -> cmpPosition phy w a b
        (False, False) -> compare a b
        (True, False)  -> LT
        (False, True)  -> GT
  where
    onScreen w =  S.current w : S.visible w
    isOnScreen a w  = a `elem` map (S.tag . S.workspace) (onScreen w)
    tagToSid s x = S.screen $ fromJust $ find ((== x) . S.tag . S.workspace) s
    cmpPosition False w a b = comparing (tagToSid $ onScreen w) a b
    cmpPosition True w a b = comparing (rect.(tagToSid $ onScreen w)) a b
      where rect i = let (Rectangle x y _ _) = screens !! fromIntegral i in (y,x)
            screens = map (screenRect . S.screenDetail) $ sortBy (comparing S.screen) $ S.current w : S.visible w

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

-- | Like 'getSortByXineramaRule', but uses physical locations for screens.
getSortByXineramaPhysicalRule :: X WorkspaceSort
getSortByXineramaPhysicalRule = mkWsSort getXineramaPhysicalWsCompare

