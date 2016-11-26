{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicWorkspaceNames
-- Copyright   :  (c) Ivan A. Malison
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <IvanMalison@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Automatically label workspaces using the WorkspaceNamesModule.
-----------------------------------------------------------------------------
module XMonad.Actions.DynamicWorkspaceNames
  (
   NameWorkspacesHook(..),
   currentWSName,
   updateWorkspaceName,
   setWorkspaceNames,
   getClass,
   getNamesFromWindows,
   getClassesNameForWorkspace
  ) where

import           Data.List
import           Data.Maybe
import           Graphics.X11.Xlib.Extras
import           Control.Monad

import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Actions.WorkspaceNames
import           XMonad.Layout.LayoutModifier

-- | Get the current name for the given workspace.
currentWSName :: WindowSpace -> X String
currentWSName ws = fromMaybe "" <$> (getWorkspaceNames' <*> pure (W.tag ws))

-- | Update the name of a workspace only if it is not the same as the current
-- name.
updateWorkspaceName :: (WindowSpace -> X String) -> WindowSpace -> X ()
updateWorkspaceName nameWorkspace workspace = do
  currentName <- currentWSName workspace
  newName <- nameWorkspace workspace
  when (currentName /= newName) $ setWorkspaceName (W.tag workspace) newName

-- | Set all workspaces names using the given workspace naming action.
setWorkspaceNames :: (WindowSpace -> X String) -> X ()
setWorkspaceNames nameWorkspace =
  gets windowset >>= mapM_ (updateWorkspaceName nameWorkspace) . W.workspaces

-- | Get the class hint for the current window
getClass :: Window -> X String
getClass w = fmap resClass $ withDisplay $ io . flip getClassHint w

-- | Apply the given action to each window in the given workspace.
getNamesFromWindows :: (Window -> X String) -> WindowSpace -> X [String]
getNamesFromWindows windowNamer w = mapM windowNamer $ W.integrate' $ W.stack w

-- | Build a '|' delimited string combining the class hints of the windows in
-- each workspace.
getClassesNameForWorkspace :: WindowSpace -> X String
getClassesNameForWorkspace = (intercalate "|" <$>) . getNamesFromWindows getClass

-- | A layout modifier that updates the names of each workspace according to
-- some function each time the layout is changed. Ideally this type would have
-- field containing the function to name workspaces, but because all
-- LayoutModifier instances must implement Show and Read, this is not currently
-- possible.
data NameWorkspacesHook a = NameWorkspacesByWindowClasses deriving (Show, Read)

instance LayoutModifier NameWorkspacesHook Window where
  hook NameWorkspacesByWindowClasses = setWorkspaceNames getClassesNameForWorkspace
