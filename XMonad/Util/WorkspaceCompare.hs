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

module XMonad.Util.WorkspaceCompare ( getWsIndex, getWsCompare, getSortByTag ) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import Data.Monoid

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

-- | Sort several workspaces according to the order in getWsCompare
getSortByTag :: X ([WindowSpace] -> [WindowSpace])
getSortByTag = do
    cmp <- getWsCompare
    return $ sortBy (\a b -> cmp (S.tag a) (S.tag b))
