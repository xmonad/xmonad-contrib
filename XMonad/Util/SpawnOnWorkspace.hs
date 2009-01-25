-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnOnWorkspace
-- Copyright   :  (c) 2009 Daniel Schoepe
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <asgaroth_@gmx.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a way to spawn an application on a specific workspace by using
-- the _NET_WM_PID property that most windows set on creation. Hence this module
-- won't work on applications that don't set this property.
--
-----------------------------------------------------------------------------
module XMonad.Util.SpawnOnWorkspace (
                                     -- * Usage
                                     -- $usage
                                     
                                     -- * Documentation
                                     -- $documentation

                                     spawnOnWorkspace,
                                     spawnOnWorkspaceHook,
                                     mkSpawnHelper,
                                     SpawnHelper
                                    ) where
import XMonad
import XMonad.Hooks.ManageHelpers
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import System.Posix.Types

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Util.SpawnOnWorkspace
--
-- >    main = do
-- >      sh <- mkSpawnHelper
-- >      ..
--
-- Then you need to add 'spawnOnWorkspaceHook' to your manage hook:
--
-- >    manageHook = spawnOnWorkspaceHook sh <+> manageHook defaultConfig
--
-- To spawn an application on a specific workspace, add a keybinding:
--
-- >        ((mod1Mask,xK_o), spawnOnWorkspace sh "urxvt" "3")
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--

-------------------------------------------------------------------

-- $documentation

-- | This structure holds the process ids and corresponding
-- workspaces for processes created with 'spawnOnWorkspace'
type SpawnHelper = IORef (M.Map ProcessID WorkspaceId)

-- | Creates a new spawn helper.
mkSpawnHelper :: IO SpawnHelper
mkSpawnHelper = newIORef M.empty

-- | Provides a manage hook to react on process spawned with
-- 'spawnOnWorkspace'.
spawnOnWorkspaceHook :: SpawnHelper -> ManageHook
spawnOnWorkspaceHook sh = do
  pd <- fromMaybe (-1) `fmap` pid
  table <- io $ readIORef sh
  case M.lookup pd table of
    Just ws -> io (modifyIORef sh (M.delete pd)) >> doShift ws
    Nothing -> doF id

-- | Spawns a process on the specified workspace.
spawnOnWorkspace :: SpawnHelper -> String -> WorkspaceId -> X ()
spawnOnWorkspace sh cmd ws = spawnPID cmd >>= io . modifyIORef sh . flip M.insert ws
