-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicWorkspaceGroups
-- Description :  Dynamically manage workspace groups in multi-head setups.
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
     -- * TopicSpace Integration
     -- $topics
    , viewTopicGroup
    , promptTopicGroupView
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as M

import XMonad
import XMonad.Prelude (find, for_)
import qualified XMonad.StackSet as W

import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.TopicSpace

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

newtype WSGroupStorage = WSG { unWSG :: M.Map WSGroupId WSGroup }
  deriving (Read, Show)

withWSG :: (M.Map WSGroupId WSGroup -> M.Map WSGroupId WSGroup) -> WSGroupStorage -> WSGroupStorage
withWSG f = WSG . f . unWSG

instance ExtensionClass WSGroupStorage where
  initialValue = WSG M.empty
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
  for_ wmap (addRawWSGroup name)
 where strength (ma, b) = ma >>= \a -> return (a,b)

-- | Give a name to the current workspace group.
addCurrentWSGroup :: WSGroupId -> X ()
addCurrentWSGroup name = withWindowSet $ \w ->
  addWSGroup name $ map (W.tag . W.workspace) (reverse $ W.current w : W.visible w)

-- | Delete the named workspace group from the list of workspace
--   groups.  Note that this has no effect on the workspaces involved;
--   it simply forgets the given name.
forgetWSGroup :: WSGroupId -> X ()
forgetWSGroup = XS.modify . withWSG . M.delete

-- | View the workspace group with the given name.
viewWSGroup :: WSGroupId -> X ()
viewWSGroup = viewGroup (windows . W.greedyView)

-- | Internal function for viewing a group.
viewGroup :: (WorkspaceId -> X ()) -> WSGroupId -> X ()
viewGroup fview name = do
  WSG m <- XS.get
  for_ (M.lookup name m) $
    mapM_ (uncurry (viewWS fview))

-- | View the given workspace on the given screen, using the provided function.
viewWS :: (WorkspaceId -> X ())  -> ScreenId -> WorkspaceId -> X ()
viewWS fview sid wid = do
  mw <- findScreenWS sid
  case mw of
    Just w -> do
      windows $ W.view w
      fview wid
    Nothing -> return ()

-- | Find the workspace which is currently on the given screen.
findScreenWS :: ScreenId -> X (Maybe WorkspaceId)
findScreenWS sid = withWindowSet $
  return . fmap (W.tag . W.workspace) . find ((==sid) . W.screen) . W.screens

newtype WSGPrompt = WSGPrompt String

instance XPrompt WSGPrompt where
  showXPrompt (WSGPrompt s) = s

-- | Prompt for a workspace group to view.
promptWSGroupView :: XPConfig -> String -> X ()
promptWSGroupView = promptGroupView viewWSGroup

-- | Internal function for making a prompt to view a workspace group
promptGroupView :: (WSGroupId -> X ()) -> XPConfig -> String -> X ()
promptGroupView fview xp s = do
  gs <- fmap (M.keys . unWSG) XS.get
  mkXPrompt (WSGPrompt s) xp (mkComplFunFromList' xp gs) fview

-- | Prompt for a name for the current workspace group.
promptWSGroupAdd :: XPConfig -> String -> X ()
promptWSGroupAdd xp s =
  mkXPrompt (WSGPrompt s) xp (const $ return []) addCurrentWSGroup

-- | Prompt for a workspace group to forget.
promptWSGroupForget :: XPConfig -> String -> X ()
promptWSGroupForget xp s = do
  gs <- fmap (M.keys . unWSG) XS.get
  mkXPrompt (WSGPrompt s) xp (mkComplFunFromList' xp gs) forgetWSGroup

-- $topics
-- You can use this module with "XMonad.Actions.TopicSpace" — just replace
-- 'promptWSGroupView' with 'promptTopicGroupView':
--
-- >    , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ")
-- >    , ("M-y g", promptTopicGroupView myTopicConfig myXPConfig "Go to group: ")
-- >    , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ")
--
-- It's also a good idea to replace 'spawn' with
-- 'XMonad.Actions.SpawnOn.spawnOn' or 'XMonad.Actions.SpawnOn.spawnHere' in
-- your topic actions, so everything is spawned where it should be.

-- | Prompt for a workspace group to view, treating the workspaces as topics.
promptTopicGroupView :: TopicConfig -> XPConfig -> String -> X ()
promptTopicGroupView = promptGroupView . viewTopicGroup

-- | View the workspace group with the given name, treating the workspaces as
-- topics.
viewTopicGroup :: TopicConfig -> WSGroupId -> X ()
viewTopicGroup = viewGroup . switchTopic
