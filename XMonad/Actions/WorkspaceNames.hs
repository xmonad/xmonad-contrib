-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WorkspaceNames
-- Copyright   :  (c) Tomas Janousek <tomi@nomi.cz>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Tomas Janousek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unportable
--
-- Provides bindings to rename workspaces, show these names in DynamicLog and
-- swap workspaces along with their names. These names survive restart.
-- Together with "XMonad.Layout.WorkspaceDir" this provides for a fully
-- dynamic topic space workflow.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Actions.WorkspaceNames (
    -- * Usage
    -- $usage

    -- * Workspace naming
    renameWorkspace,
    workspaceNamesPP,
    getWorkspaceNames',
    getWorkspaceNames,
    getWorkspaceName,
    getCurrentWorkspaceName,
    setWorkspaceName,
    setCurrentWorkspaceName,

    -- * Workspace swapping
    swapTo,
    swapTo',
    swapWithCurrent,

    -- * Workspace prompt
    workspaceNamePrompt,

    -- * EwmhDesktops integration
    workspaceNamesListTransform
    ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.CycleWS (findWorkspace, WSType(..), Direction1D(..))
import qualified XMonad.Actions.SwapWorkspaces as Swap
import XMonad.Hooks.DynamicLog (PP(..))
import XMonad.Prompt (mkXPrompt, XPConfig)
import XMonad.Prompt.Workspace (Wor(Wor))
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.WorkspaceNames
--
-- Then add keybindings like the following:
--
-- >   , ((modm .|. shiftMask, xK_r      ), renameWorkspace def)
--
-- and apply workspaceNamesPP to your DynamicLog pretty-printer:
--
-- > myLogHook =
-- >     workspaceNamesPP xmobarPP >>= dynamicLogString >>= xmonadPropLog
--
-- We also provide a modification of "XMonad.Actions.SwapWorkspaces"\'s
-- functionality, which may be used this way:
--
-- >   , ((modMask .|. shiftMask, xK_Left  ), swapTo Prev)
-- >   , ((modMask .|. shiftMask, xK_Right ), swapTo Next)
--
-- > [((modm .|. controlMask, k), swapWithCurrent i)
-- >     | (i, k) <- zip workspaces [xK_1 ..]]
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".



-- | Workspace names container.
newtype WorkspaceNames = WorkspaceNames (M.Map WorkspaceId String)
    deriving (Typeable, Read, Show)

instance ExtensionClass WorkspaceNames where
    initialValue = WorkspaceNames M.empty
    extensionType = PersistentExtension

-- | Returns a lookup function that maps workspace tags to workspace names.
getWorkspaceNames' :: X (WorkspaceId -> Maybe String)
getWorkspaceNames' = do
    WorkspaceNames m <- XS.get
    return (`M.lookup` m)

-- | Returns a function for 'ppRename' that appends @sep@ and the workspace
-- name, if set.
getWorkspaceNames :: String -> X (String -> WindowSpace -> String)
getWorkspaceNames sep = ren <$> getWorkspaceNames'
  where
    ren name s w = s ++ maybe "" (sep ++) (name (W.tag w))

-- | Gets the name of a workspace, if set, otherwise returns nothing.
getWorkspaceName :: WorkspaceId -> X (Maybe String)
getWorkspaceName w = ($ w) <$> getWorkspaceNames'

-- | Gets the name of the current workspace. See 'getWorkspaceName'
getCurrentWorkspaceName :: X (Maybe String)
getCurrentWorkspaceName = getWorkspaceName =<< gets (W.currentTag . windowset)

-- | Sets the name of a workspace. Empty string makes the workspace unnamed
-- again.
setWorkspaceName :: WorkspaceId -> String -> X ()
setWorkspaceName w name = do
    WorkspaceNames m <- XS.get
    XS.put $ WorkspaceNames $ if null name then M.delete w m else M.insert w name m
    refresh

-- | Sets the name of the current workspace. See 'setWorkspaceName'.
setCurrentWorkspaceName :: String -> X ()
setCurrentWorkspaceName name = do
    current <- gets (W.currentTag . windowset)
    setWorkspaceName current name

-- | Prompt for a new name for the current workspace and set it.
renameWorkspace :: XPConfig -> X ()
renameWorkspace conf =
    mkXPrompt pr conf (const (return [])) setCurrentWorkspaceName
    where pr = Wor "Workspace name: "

-- | Modify "XMonad.Hooks.DynamicLog"\'s pretty-printing format to show
-- workspace names as well.
workspaceNamesPP :: PP -> X PP
workspaceNamesPP pp = getWorkspaceNames ":" <&> \ren -> pp{ ppRename = ppRename pp >=> ren }

-- | See 'XMonad.Actions.SwapWorkspaces.swapTo'. This is the same with names.
swapTo :: Direction1D -> X ()
swapTo dir = swapTo' dir AnyWS

-- | Swap with the previous or next workspace of the given type.
swapTo' :: Direction1D -> WSType -> X ()
swapTo' dir which = findWorkspace getSortByIndex dir which 1 >>= swapWithCurrent

-- | See 'XMonad.Actions.SwapWorkspaces.swapWithCurrent'. This is almost the
-- same with names.
swapWithCurrent :: WorkspaceId -> X ()
swapWithCurrent t = do
    current <- gets (W.currentTag . windowset)
    swapNames t current
    windows $ Swap.swapWorkspaces t current

-- | Swap names of the two workspaces.
swapNames :: WorkspaceId -> WorkspaceId -> X ()
swapNames w1 w2 = do
    WorkspaceNames m <- XS.get
    let getname w = fromMaybe "" $ M.lookup w m
        set w name m' = if null name then M.delete w m' else M.insert w name m'
    XS.put $ WorkspaceNames $ set w1 (getname w2) $ set w2 (getname w1) $ m

-- | Same behavior than 'XMonad.Prompt.Workspace.workspacePrompt' excepted it acts on the workspace name provided by this module.
workspaceNamePrompt :: XPConfig -> (WorkspaceId -> X ()) -> X ()
workspaceNamePrompt conf job = do
    myWorkspaces <- gets $ W.workspaces . windowset
    myWorkspacesName <- getWorkspaceNames ":" <&> \n -> [n (W.tag w) w | w <- myWorkspaces]
    let pairs = zip myWorkspacesName (map W.tag myWorkspaces)
    mkXPrompt (Wor "Select workspace: ") conf
              (contains myWorkspacesName)
              (job . toWsId pairs)
  where toWsId pairs name = fromMaybe "" (lookup name pairs)
        contains completions input =
          return $ filter (Data.List.isInfixOf input) completions

-- | Workspace list transformation for
-- 'XMonad.Hooks.EwmhDesktops.ewmhDesktopsLogHookCustom' that exposes
-- workspace names to pagers and other EWMH-aware clients.
--
-- Usage:
-- > logHook = (workspaceNamesListTransform >>= ewmhDesktopsLogHookCustom) <+> …
workspaceNamesListTransform :: X ([WindowSpace] -> [WindowSpace])
workspaceNamesListTransform =
    getWorkspaceNames ":" <&> \names -> map $ \ws -> ws{ W.tag = names (W.tag ws) ws }
