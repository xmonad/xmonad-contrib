{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PerWorkspace
-- Copyright   :  (c) Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts on a per-workspace basis.
-----------------------------------------------------------------------------

module XMonad.Layout.PerWorkspace
    ( -- * Usage
      -- $usage
      onWorkspace, onWorkspaces
    ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe (fromMaybe)

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.PerWorkspace
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = onWorkspace "foo" l1 $  -- layout l1 will be used on workspace "foo".
-- >              onWorkspaces ["bar","6"] l2 $  -- layout l2 will be used on workspaces "bar" and "6".
-- >              l3                      -- layout l3 will be used on all other workspaces.
--
-- Note that @l1@, @l2@, and @l3@ can be arbitrarily complicated layouts,
-- e.g. @(Full ||| smartBorders $ tabbed shrinkText defaultTConf ||| ...)@
--
-- In another scenario, suppose you wanted to have layouts A, B, and C
-- available on all workspaces, except that on workspace foo you want
-- layout D instead of C.  You could do that as follows:
--
-- > layoutHook = A ||| B ||| onWorkspace "foo" D C

-- | Specify one layout to use on a particular workspace, and another
--   to use on all others.  The second layout can be another call to
--   'onWorkspace', and so on.
onWorkspace :: (LayoutClass l1 a, LayoutClass l2 a)
               => WorkspaceId -- ^ the tag of the workspace to match
               -> (l1 a)      -- ^ layout to use on the matched workspace
               -> (l2 a)      -- ^ layout to use everywhere else
               -> PerWorkspace l1 l2 a
onWorkspace wsId l1 l2 = PerWorkspace [wsId] False l1 l2

-- | Specify one layout to use on a particular set of workspaces, and
--   another to use on all other workspaces.
onWorkspaces :: (LayoutClass l1 a, LayoutClass l2 a)
                => [WorkspaceId]  -- ^ tags of workspaces to match
                -> (l1 a)         -- ^ layout to use on matched workspaces
                -> (l2 a)         -- ^ layout to use everywhere else
                -> PerWorkspace l1 l2 a
onWorkspaces wsIds l1 l2 = PerWorkspace wsIds False l1 l2

-- | Structure for representing a workspace-specific layout along with
-- a layout for all other workspaces. We store the tags of workspaces
-- to be matched, and the two layouts. We save the layout choice in
-- the Bool, to be used to implement description.
data PerWorkspace l1 l2 a = PerWorkspace [WorkspaceId]
                                         Bool
                                         (l1 a)
                                         (l2 a)
    deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (PerWorkspace l1 l2) a where
    runLayout (W.Workspace i p@(PerWorkspace wsIds _ lt lf) ms) r
        | i `elem` wsIds = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                              return (wrs, Just $ mkNewPerWorkspaceT p mlt')
        | otherwise      = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                              return (wrs, Just $ mkNewPerWorkspaceF p mlt')

    handleMessage (PerWorkspace wsIds bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ PerWorkspace wsIds bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ PerWorkspace wsIds bool lt nf)

    description (PerWorkspace _ True  l1 _) = description l1
    description (PerWorkspace _ _     _ l2) = description l2

-- | Construct new PerWorkspace values with possibly modified layouts.
mkNewPerWorkspaceT :: PerWorkspace l1 l2 a -> Maybe (l1 a) ->
                      PerWorkspace l1 l2 a
mkNewPerWorkspaceT (PerWorkspace wsIds _ lt lf) mlt' =
    (\lt' -> PerWorkspace wsIds True lt' lf) $ fromMaybe lt mlt'

mkNewPerWorkspaceF :: PerWorkspace l1 l2 a -> Maybe (l2 a) ->
                      PerWorkspace l1 l2 a
mkNewPerWorkspaceF (PerWorkspace wsIds _ lt lf) mlf' =
    (\lf' -> PerWorkspace wsIds False lt lf') $ fromMaybe lf mlf'
