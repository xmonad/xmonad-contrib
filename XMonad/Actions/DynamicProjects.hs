--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.DynamicProjects
-- Description :  Treat workspaces as individual project areas.
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
       , changeProjectDirPrompt

         -- * Helper Functions
       , switchProject
       , shiftToProject
       , lookupProject
       , currentProject
       , activateProject
       , modifyProject
       ) where

--------------------------------------------------------------------------------
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Directory (setCurrentDirectory, getHomeDirectory, makeAbsolute)
import XMonad.Prelude
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt
import XMonad.Prompt.Directory
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
-- or further configure the workspace/project. To close a project,
-- you can use the functions provided by "XMonad.Actions.DynamicWorkspaces",
-- such as @removeWorkspace@ or @removeWorkspaceByTag@.
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
-- >  , ((modm, xK_space), switchProjectPrompt def)
-- >  , ((modm, xK_slash), shiftToProjectPrompt def)
--
-- For detailed instructions on editing your key bindings, see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.

--------------------------------------------------------------------------------
type ProjectName  = String
type ProjectTable = Map ProjectName Project

--------------------------------------------------------------------------------
-- | Details about a workspace that represents a project.
data Project = Project
  { projectName      :: !ProjectName    -- ^ Workspace name.
  , projectDirectory :: !FilePath       -- ^ Working directory.
  , projectStartHook :: !(Maybe (X ())) -- ^ Optional start-up hook.
  }

--------------------------------------------------------------------------------
-- | Internal project state.
data ProjectState = ProjectState
  { projects        :: !ProjectTable
  , previousProject :: !(Maybe WorkspaceId)
  }

--------------------------------------------------------------------------------
instance ExtensionClass ProjectState where
  initialValue = ProjectState Map.empty Nothing

--------------------------------------------------------------------------------
-- Internal types for working with XPrompt.
data ProjectPrompt = ProjectPrompt XPConfig ProjectMode [ProjectName]
data ProjectMode = SwitchMode | ShiftMode | RenameMode | DirMode

instance XPrompt ProjectPrompt where
  showXPrompt (ProjectPrompt _ submode _) =
    case submode of
      SwitchMode -> "Switch or Create Project: "
      ShiftMode  -> "Send Window to Project: "
      RenameMode -> "New Project Name: "
      DirMode    -> "Change Project Directory: "

  completionFunction (ProjectPrompt _ RenameMode _) = return . (:[])
  completionFunction (ProjectPrompt c DirMode _) =
    let xpt = directoryMultipleModes' (complCaseSensitivity c) "" (const $ return ())
    in completionFunction xpt
  completionFunction (ProjectPrompt c _ ns) = mkComplFunFromList' c ns

  modeAction (ProjectPrompt _ SwitchMode _) buf auto = do
    let name = if null auto then buf else auto
    ps <- XS.gets projects

    case Map.lookup name ps of
      Just p              -> switchProject p
      Nothing | null name -> return ()
              | otherwise -> switchProject (defProject name)

  modeAction (ProjectPrompt _ ShiftMode _) buf auto = do
    let name = if null auto then buf else auto
    ps <- XS.gets projects
    shiftToProject . fromMaybe (defProject name) $ Map.lookup name ps

  modeAction (ProjectPrompt _ RenameMode _) name _ =
    when (not (null name) && not (all isSpace name)) $ do
      renameWorkspaceByName name
      modifyProject (\p -> p { projectName = name })

  modeAction (ProjectPrompt _ DirMode _) buf auto = do
    let dir' = if null auto then buf else auto
    dir <- io $ makeAbsolute dir'
    modifyProject (\p -> p { projectDirectory = dir })

--------------------------------------------------------------------------------
-- | Add dynamic projects support to the given config.
dynamicProjects :: [Project] -> XConfig a -> XConfig a
dynamicProjects ps c =
  c { startupHook     = dynamicProjectsStartupHook ps <> startupHook c
    , logHook         = dynamicProjectsLogHook        <> logHook c
    }

--------------------------------------------------------------------------------
-- | Log hook for tracking workspace changes.
dynamicProjectsLogHook :: X ()
dynamicProjectsLogHook = do
  name   <- gets (W.tag . W.workspace . W.current . windowset)
  xstate <- XS.get

  unless (Just name == previousProject xstate) $ do
    XS.put (xstate {previousProject = Just name})
    activateProject . fromMaybe (defProject name) $
      Map.lookup name (projects xstate)

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
lookupProject name = Map.lookup name <$> XS.gets projects

--------------------------------------------------------------------------------
-- | Fetch the current project (the one being used for the currently
-- active workspace). If the workspace doesn't have a project, a
-- default project is returned, using the workspace name as the
-- project name.
currentProject :: X Project
currentProject = do
  name <- gets (W.tag . W.workspace . W.current . windowset)
  proj <- lookupProject name
  return $ fromMaybe (defProject name) proj

--------------------------------------------------------------------------------
-- | Modify the current project using a pure function.
modifyProject :: (Project -> Project) -> X ()
modifyProject f = do
  p  <- currentProject
  ps <- XS.gets projects

  -- If a project is renamed to match another project, the old project
  -- will be removed and replaced with this one.
  let new = f p
      ps' = Map.insert (projectName new) new $ Map.delete (projectName p) ps

  XS.modify $ \s -> s {projects = ps'}
  activateProject new

--------------------------------------------------------------------------------
-- | Switch to the given project.
switchProject :: Project -> X ()
switchProject p = appendWorkspace (projectName p)

--------------------------------------------------------------------------------
-- | Prompt for a project name and then switch to it.  Automatically
-- creates a project if a new name is returned from the prompt.
switchProjectPrompt :: XPConfig -> X ()
switchProjectPrompt = projectPrompt [ SwitchMode
                                    , ShiftMode
                                    , RenameMode
                                    , DirMode
                                    ]

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
shiftToProjectPrompt = projectPrompt [ ShiftMode
                                     , RenameMode
                                     , SwitchMode
                                     , DirMode
                                     ]

--------------------------------------------------------------------------------
-- | Rename the current project.
renameProjectPrompt :: XPConfig -> X ()
renameProjectPrompt = projectPrompt [ RenameMode
                                    , DirMode
                                    , SwitchMode
                                    , ShiftMode
                                    ]

--------------------------------------------------------------------------------
-- | Change the working directory used for the current project.
--
-- NOTE: This will only affect new processed started in this project.
-- Existing processes will maintain the previous working directory.
changeProjectDirPrompt :: XPConfig -> X ()
changeProjectDirPrompt = projectPrompt [ DirMode
                                       , SwitchMode
                                       , ShiftMode
                                       , RenameMode
                                       ]

--------------------------------------------------------------------------------
-- | Prompt for a project name.
projectPrompt :: [ProjectMode] -> XPConfig -> X ()
projectPrompt submodes c = do
  ws <- map W.tag <$> gets (W.workspaces . windowset)
  ps <- XS.gets projects

  let names = sort (Map.keys ps `union` ws)
      modes = map (\m -> XPT $ ProjectPrompt c m names) submodes

  mkXPromptWithModes modes c

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
