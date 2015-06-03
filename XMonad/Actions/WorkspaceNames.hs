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
    getWorkspaceNames,
    setWorkspaceName,
    setCurrentWorkspaceName,

    -- * Workspace swapping
    swapTo,
    swapTo',
    swapWithCurrent,

    -- * Workspace prompt
    workspaceNamePrompt
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

-- | Returns a function that maps workspace tag @\"t\"@ to @\"t:name\"@ for
-- workspaces with a name, and to @\"t\"@ otherwise.
getWorkspaceNames :: X (WorkspaceId -> String)
getWorkspaceNames = do
    WorkspaceNames m <- XS.get
    return $ \wks -> case M.lookup wks m of
        Nothing -> wks
        Just s  -> wks ++ ":" ++ s

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
renameWorkspace conf = do
    mkXPrompt pr conf (const (return [])) setCurrentWorkspaceName
    where pr = Wor "Workspace name: "

-- | Modify "XMonad.Hooks.DynamicLog"\'s pretty-printing format to show
-- workspace names as well.
workspaceNamesPP :: PP -> X PP
workspaceNamesPP pp = do
    names <- getWorkspaceNames
    return $
        pp {
            ppCurrent         = ppCurrent         pp . names,
            ppVisible         = ppVisible         pp . names,
            ppHidden          = ppHidden          pp . names,
            ppHiddenNoWindows = ppHiddenNoWindows pp . names,
            ppUrgent          = ppUrgent          pp . names
        }

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
workspaceNamePrompt :: XPConfig -> (String -> X ()) -> X ()
workspaceNamePrompt conf job = do
    myWorkspaces <- gets $ map W.tag . W.workspaces . windowset
    myWorkspacesName <- getWorkspaceNames >>= \f -> return $ map f myWorkspaces
    let pairs = zip myWorkspacesName myWorkspaces
    mkXPrompt (Wor "Select workspace: ") conf
              (contains myWorkspacesName)
              (job . toWsId pairs)
  where toWsId pairs name = case lookup name pairs of
                                Nothing -> ""
                                Just i -> i
        contains completions input =
          return $ filter (Data.List.isInfixOf input) completions
