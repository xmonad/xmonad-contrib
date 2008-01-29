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
-- Configure layouts on a per-workspace basis.  NOTE that this module
-- does not (yet) work in conjunction with multiple screens! =(
--
-- Note also that when using PerWorkspace, on initial startup workspaces
-- may not respond to messages properly until a window has been opened.
-- This is due to a limitation inherent in the way PerWorkspace is
-- implemented: it cannot decide which layout to use until actually
-- required to lay out some windows (which does not happen until a window
-- is opened).
-----------------------------------------------------------------------------

module XMonad.Layout.PerWorkspace (
                                    -- * Usage
                                    -- $usage

                                    onWorkspace, onWorkspaces
                                  ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutCombinators
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
--
-- NOTE that this module does not (yet) work in conjunction with
-- multiple screens. =(

-- | Specify one layout to use on a particular workspace, and another
--   to use on all others.  The second layout can be another call to
--   'onWorkspace', and so on.
onWorkspace :: WorkspaceId  -- ^ tags of workspaces to match
                -> (l1 a)         -- ^ layout to use on matched workspaces
                -> (l2 a)         -- ^ layout to use everywhere else
                -> CombinedLayout PerWorkspace l1 l2 a
onWorkspace wsId l1 l2 = CombinedLayout (PerWorkspace [wsId]) l1 l2

-- | Specify one layout to use on a particular set of workspaces, and
--   another to use on all other workspaces.
onWorkspaces :: [WorkspaceId]  -- ^ tags of workspaces to match
                -> (l1 a)         -- ^ layout to use on matched workspaces
                -> (l2 a)         -- ^ layout to use everywhere else
                -> CombinedLayout PerWorkspace l1 l2 a
onWorkspaces wsIds l1 l2 = CombinedLayout (PerWorkspace wsIds) l1 l2

-- | Structure for representing a workspace-specific layout along with
--   a layout for all other workspaces.  We store the tags of workspaces
--   to be matched, and the two layouts.  Since layouts are stored\/tracked
--   per workspace, once we figure out whether we're on a matched workspace,
--   we can cache that information using a (Maybe Bool).  This is necessary
--   to be able to correctly implement the 'description' method of
--   LayoutClass, since a call to description is not able to query the
--   WM state to find out which workspace it was called in.
data PerWorkspace a = PerWorkspace [WorkspaceId] deriving (Read, Show)

instance LayoutCombinator PerWorkspace a where
    chooser (PerWorkspace wsIds) = do
        t <- getCurrentTag
        return $ if t `elem` wsIds then DoFirst else DoSecond
    doFirst (PerWorkspace _) = True

-- | Get the tag of the currently active workspace.  Note that this
--   is only guaranteed to be the same workspace for which doLayout
--   was called if there is only one screen.
getCurrentTag :: X WorkspaceId
getCurrentTag = gets windowset >>= return . W.tag . W.workspace . W.current
