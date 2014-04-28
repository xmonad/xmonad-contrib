{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicWorkspaceGroups
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
-- Dynamically manage \"workspace groups\", sets of workspaces being
-- used together for some common task or purpose, to allow switching
-- between workspace groups in a single action.  Note that this only
-- makes sense for multi-head setups.
--
-----------------------------------------------------------------------------

module XMonad.Actions.DynamicWorkspaceGroups
    ( -- * Usage
      -- $usage

      WSGroupId

    , addRawWSGroup
    , addWSGroup
    , addCurrentWSGroup
    , forgetWSGroup
    , viewWSGroup

    , promptWSGroupView
    , promptWSGroupAdd
    , promptWSGroupForget

    , WSGPrompt
    ) where

import Data.List (find)
import Control.Arrow ((&&&))
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Actions.DynamicWorkspaceGroups
--
-- Then add keybindings like the following (this example uses
-- "XMonad.Util.EZConfig"-style keybindings, but this is not necessary):
--
-- >    , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ")
-- >    , ("M-y g", promptWSGroupView myXPConfig "Go to group: ")
-- >    , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ")
--

type WSGroup = [(ScreenId,WorkspaceId)]

type WSGroupId = String

data WSGroupStorage = WSG { unWSG :: M.Map WSGroupId WSGroup }
  deriving (Typeable, Read, Show)

withWSG :: (M.Map WSGroupId WSGroup -> M.Map WSGroupId WSGroup) -> WSGroupStorage -> WSGroupStorage
withWSG f = WSG . f . unWSG

instance ExtensionClass WSGroupStorage where
  initialValue = WSG $ M.empty
  extensionType = PersistentExtension

-- | Add a new workspace group of the given name, mapping to an
--   explicitly specified association between screen IDs and workspace
--   names.  This function could be useful for, say, creating some
--   standard workspace groups in your startup hook.
addRawWSGroup :: WSGroupId -> [(ScreenId, WorkspaceId)] -> X ()
addRawWSGroup name = XS.modify . withWSG . M.insert name

-- | Add a new workspace group with the given name.
addWSGroup :: WSGroupId -> [WorkspaceId] -> X ()
addWSGroup name wids = withWindowSet $ \w -> do
  let wss  = map ((W.tag . W.workspace) &&& W.screen) $ W.screens w
      wmap = mapM (strength . (flip lookup wss &&& id)) wids
  case wmap of
    Just ps -> addRawWSGroup name ps
    Nothing -> return ()
 where strength (ma, b) = ma >>= \a -> return (a,b)

-- | Give a name to the current workspace group.
addCurrentWSGroup :: WSGroupId -> X ()
addCurrentWSGroup name = withWindowSet $ \w ->
  addWSGroup name $ map (W.tag . W.workspace) (W.current w : W.visible w)

-- | Delete the named workspace group from the list of workspace
--   groups.  Note that this has no effect on the workspaces involved;
--   it simply forgets the given name.
forgetWSGroup :: WSGroupId -> X ()
forgetWSGroup = XS.modify . withWSG . M.delete

-- | View the workspace group with the given name.
viewWSGroup :: WSGroupId -> X ()
viewWSGroup name = do
  WSG m <- XS.get
  case M.lookup name m of
    Just grp -> mapM_ (uncurry viewWS) grp
    Nothing -> return ()

-- | View the given workspace on the given screen.
viewWS :: ScreenId -> WorkspaceId -> X ()
viewWS sid wid = do
  mw <- findScreenWS sid
  case mw of
    Just w -> do
      windows $ W.view w
      windows $ W.greedyView wid
    Nothing -> return ()

-- | Find the workspace which is currently on the given screen.
findScreenWS :: ScreenId -> X (Maybe WorkspaceId)
findScreenWS sid = withWindowSet $
  return . fmap (W.tag . W.workspace) . find ((==sid) . W.screen) . W.screens

data WSGPrompt = WSGPrompt String

instance XPrompt WSGPrompt where
  showXPrompt (WSGPrompt s) = s

-- | Prompt for a workspace group to view.
promptWSGroupView :: XPConfig -> String -> X ()
promptWSGroupView xp s = do
  gs <- fmap (M.keys . unWSG) XS.get
  mkXPrompt (WSGPrompt s) xp (mkComplFunFromList' gs) viewWSGroup

-- | Prompt for a name for the current workspace group.
promptWSGroupAdd :: XPConfig -> String -> X ()
promptWSGroupAdd xp s =
  mkXPrompt (WSGPrompt s) xp (const $ return []) addCurrentWSGroup

-- | Prompt for a workspace group to forget.
promptWSGroupForget :: XPConfig -> String -> X ()
promptWSGroupForget xp s = do
  gs <- fmap (M.keys . unWSG) XS.get
  mkXPrompt (WSGPrompt s) xp (mkComplFunFromList' gs) forgetWSGroup
