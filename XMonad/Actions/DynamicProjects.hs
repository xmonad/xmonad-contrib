{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicProjects
-- Copyright   :  (c) Peter J. Jones
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Peter Jones <pjones@devalot.com>
-- Stability   :  unstable
-- Portability :  not portable
--
-- Imbues workspaces with additional features so they can be treated
-- as individual project areas.
--------------------------------------------------------------------------------
module XMonad.Actions.DynamicProjects
       ( -- * Overview
         -- $overview

         -- * Usage
         -- $usage

         -- * Types
         Project (..)
       , ProjectName

         -- * Hooks
       , dynamicProjects

         -- * Bindings
       , switchProjectPrompt
       , shiftToProjectPrompt
       , renameProjectPrompt

         -- * Helper Functions
       , switchProject
       , shiftToProject
       , lookupProject
       , currentProject
       , activateProject
       ) where

--------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.List (sort, union, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import System.Directory (setCurrentDirectory, getHomeDirectory)
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Prompt.Directory (directoryPrompt)
import XMonad.Prompt.Workspace (Wor(..))
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

--------------------------------------------------------------------------------
-- $overview
-- Inspired by @TopicSpace@, @DynamicWorkspaces@, and @WorkspaceDir@,
-- @DynamicProjects@ treats workspaces as projects while maintaining
-- compatibility with all existing workspace-related functionality in
-- XMonad.
--
-- Instead of using generic workspace names such as @3@ or @work@,
-- @DynamicProjects@ allows you to dedicate workspaces to specific
-- projects and then switch between projects easily.
--
-- A project is made up of a name, working directory, and a start-up
-- hook.  When you switch to a workspace, @DynamicProjects@ changes
-- the working directory to the one configured for the matching
-- project.  If the workspace doesn't have any windows, the project's
-- start-up hook is executed.  This allows you to launch applications
-- or further configure the workspace/project.
--
-- When using the @switchProjectPrompt@ function, workspaces are
-- created as needed.  This means you can create new project spaces
-- (and therefore workspaces) on the fly.  (These dynamic projects are
-- not preserved across restarts.)
--
-- Additionally, frequently used projects can be configured statically
-- in your XMonad configuration.  Doing so allows you to configure the
-- per-project start-up hook.

--------------------------------------------------------------------------------
-- $usage
-- To use @DynamicProjects@ you need to add it to your XMonad
-- configuration and then configure some optional key bindings.
--
-- > import XMonad.Actions.DynamicProjects
--
-- Start by defining some projects:
--
-- > projects :: [Project]
-- > projects =
-- >   [ Project { projectName      = "scratch"
-- >             , projectDirectory = "~/"
-- >             , projectStartHook = Nothing
-- >             }
-- >
-- >   , Project { projectName      = "browser"
-- >             , projectDirectory = "~/download"
-- >             , projectStartHook = Just $ do spawn "conkeror"
-- >                                            spawn "chromium"
-- >             }
-- >   ]
--
-- Then inject @DynamicProjects@ into your XMonad configuration:
--
-- > main = xmonad $ dynamicProjects projects def
--
-- And finally, configure some optional key bindings:
--
-- >  , ((modm, xK_space), switchProjectPrompt)
-- >  , ((modm, xK_slash), shiftToProjectPrompt)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

--------------------------------------------------------------------------------
type ProjectName  = String
type ProjectTable = Map ProjectName Project

--------------------------------------------------------------------------------
-- | Details about a workspace that represents a project.
data Project = Project
  { projectName      :: !ProjectName    -- ^ Workspace name.
  , projectDirectory :: !FilePath       -- ^ Working directory.
  , projectStartHook :: !(Maybe (X ())) -- ^ Optional start-up hook.
  } deriving Typeable

--------------------------------------------------------------------------------
-- | Internal project state.
data ProjectState = ProjectState
  { projects        :: !ProjectTable
  , previousProject :: !(Maybe WorkspaceId)
  } deriving Typeable

--------------------------------------------------------------------------------
instance ExtensionClass ProjectState where
  initialValue = ProjectState Map.empty Nothing

--------------------------------------------------------------------------------
-- | Add dynamic projects support to the given config.
dynamicProjects :: [Project] -> XConfig a -> XConfig a
dynamicProjects ps c =
  c { startupHook     = startupHook c <> dynamicProjectsStartupHook ps
    , logHook         = logHook c     <> dynamicProjectsLogHook
    }

--------------------------------------------------------------------------------
-- | Log hook for tracking workspace changes.
dynamicProjectsLogHook :: X ()
dynamicProjectsLogHook = do
  name  <- gets (W.tag . W.workspace . W.current . windowset)
  state <- XS.get

  unless (Just name == previousProject state) $ do
    XS.put (state {previousProject = Just name})
    activateProject . fromMaybe (defProject name) $
      Map.lookup name (projects state)

--------------------------------------------------------------------------------
-- | Start-up hook for recording configured projects.
dynamicProjectsStartupHook :: [Project] -> X ()
dynamicProjectsStartupHook ps = XS.modify go
  where
    go :: ProjectState -> ProjectState
    go s = s {projects = update $ projects s}

    update :: ProjectTable -> ProjectTable
    update = Map.union (Map.fromList $ map entry ps)

    entry :: Project -> (ProjectName, Project)
    entry p = (projectName p, addDefaultHook p)

    -- Force the hook to be a @Just@ so that it doesn't automatically
    -- get deleted when switching away from a workspace with no
    -- windows.
    addDefaultHook :: Project -> Project
    addDefaultHook p = p { projectStartHook = projectStartHook p <|>
                                              Just (return ())
                         }

--------------------------------------------------------------------------------
-- | Find a project based on its name.
lookupProject :: ProjectName -> X (Maybe Project)
lookupProject name = Map.lookup name `fmap` XS.gets projects

--------------------------------------------------------------------------------
-- | Fetch the current project (the one being used for the currently
-- active workspace).
currentProject :: X Project
currentProject = do
  name <- gets (W.tag . W.workspace . W.current . windowset)
  proj <- lookupProject name
  return $ fromMaybe (defProject name) proj

--------------------------------------------------------------------------------
-- | Switch to the given project.
switchProject :: Project -> X ()
switchProject p = do
  oldws <- gets (W.workspace . W.current . windowset)
  oldp <- currentProject

  let name = W.tag oldws
      ws   = W.integrate' (W.stack oldws)

  -- If the project we are switching away from has no windows, and
  -- it's a dynamic project, remove it from the configuration.
  when (null ws && isNothing (projectStartHook oldp)) $
    XS.modify (\s -> s {projects = Map.delete name $ projects s})

  appendWorkspace (projectName p)

--------------------------------------------------------------------------------
-- | Prompt for a project name and then switch to it.  Automatically
-- creates a project if a new name is returned from the prompt.
switchProjectPrompt :: XPConfig -> X ()
switchProjectPrompt c = projectPrompt c switch
  where
    switch :: ProjectTable -> ProjectName -> X ()
    switch ps name = case Map.lookup name ps of
      Just p              -> switchProject p
      Nothing | null name -> return ()
              | otherwise -> directoryPrompt dirC "Project Dir: " (mkProject name)

    dirC :: XPConfig
    dirC = c { alwaysHighlight = False } -- Fix broken tab completion.

    mkProject :: ProjectName -> FilePath -> X ()
    mkProject name dir = do
      let p = Project name dir Nothing
      XS.modify $ \s -> s {projects = Map.insert name p $ projects s}
      switchProject p

--------------------------------------------------------------------------------
-- | Shift the currently focused window to the given project.
shiftToProject :: Project -> X ()
shiftToProject p = do
  addHiddenWorkspace (projectName p)
  windows (W.shift $ projectName p)

--------------------------------------------------------------------------------
-- | Prompts for a project name and then shifts the currently focused
-- window to that project.
shiftToProjectPrompt :: XPConfig -> X ()
shiftToProjectPrompt c = projectPrompt c go
  where
    go :: ProjectTable -> ProjectName -> X ()
    go ps name = shiftToProject . fromMaybe (defProject name) $
                 Map.lookup name ps

--------------------------------------------------------------------------------
-- | Prompt for a project name.
projectPrompt :: XPConfig -> (ProjectTable -> ProjectName -> X ()) -> X ()
projectPrompt c f = do
  ws <- map W.tag `fmap` gets (W.workspaces . windowset)
  ps <- XS.gets projects

  let names = sort (Map.keys ps `union` ws)
      label = "Switch or Create Project: "

  mkXPrompt (Wor label) c (mkComplFunFromList' names) (f ps)

--------------------------------------------------------------------------------
-- | Rename the current project.
renameProjectPrompt :: XPConfig -> X ()
renameProjectPrompt c = mkXPrompt (Wor "New Project Name: ") c (return . (:[])) go
  where
    go :: String -> X ()
    go name = do
      p  <- currentProject
      ps <- XS.gets projects
      renameWorkspaceByName name

      let p'  = fromMaybe (p { projectName = name }) $ Map.lookup name ps
          ps' = Map.insert name p' $ Map.delete (projectName p) ps

      XS.modify $ \s -> s {projects = ps'}
      activateProject p'

--------------------------------------------------------------------------------
-- | Activate a project by updating the working directory and
-- possibly running its start-up hook.  This function is automatically
-- invoked when the workspace changes.
activateProject :: Project -> X ()
activateProject p = do
    ws   <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
    home <- io getHomeDirectory

    -- Change to the project's directory.
    catchIO (setCurrentDirectory $ expandHome home $ projectDirectory p)

    -- Possibly run the project's startup hook.
    when (null ws) $ fromMaybe (return ()) (projectStartHook p)

  where

    -- Replace an initial @~@ character with the home directory.
    expandHome :: FilePath -> FilePath -> FilePath
    expandHome home dir = case stripPrefix "~" dir of
      Nothing -> dir
      Just xs -> home ++ xs

--------------------------------------------------------------------------------
-- | Default project.
defProject :: ProjectName -> Project
defProject name = Project name "~/" Nothing
