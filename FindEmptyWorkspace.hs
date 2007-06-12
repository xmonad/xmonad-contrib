-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.FindEmptyWorkspace
-- Copyright   :  (c) Miikka Koskinen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  arcatan@kapsi.fi
--
-----------------------------------------------------------------------------
--
-- Find an empty workspace in xmonad.
--
-- To use, modify your Config.hs to:
--
--     import XMonadContrib.FindEmptyWorkspace
--
-- and add a keybinding:
--
--     , ((modMask,                xK_m    ), viewEmptyWorkspace)
--     , ((modMask .|. shiftMask,  xK_m    ), tagToEmptyWorkspace)
--
-- Now you can jump to an empty workspace with mod-m. Mod-shift-m will
-- tag the current window to an empty workspace and view it.
--

module XMonadContrib.FindEmptyWorkspace (
    viewEmptyWorkspace, tagToEmptyWorkspace
  ) where

import Control.Monad.State
import Data.List
import Data.Maybe ( isNothing )

import XMonad
import StackSet

import qualified Operations as O

-- | Find the first hidden empty workspace in a StackSet. Returns
-- Nothing if all workspaces are in use. Function searches currently
-- focused workspace, other visible workspaces (when in Xinerama) and
-- hidden workspaces in this order.
findEmptyWorkspace :: StackSet i a s -> Maybe (Workspace i a)
findEmptyWorkspace = find (isNothing . stack) . allWorkspaces
  where
    allWorkspaces ss = (workspace . current) ss :
                       (map workspace . visible) ss ++ hidden ss

withEmptyWorkspace :: (WorkspaceId -> X ()) -> X ()
withEmptyWorkspace f = do
    ws <- gets windowset
    whenJust (findEmptyWorkspace ws) (f . tag)

-- | Find and view an empty workspace. Do nothing if all workspaces are
-- in use.
viewEmptyWorkspace :: X ()
viewEmptyWorkspace = withEmptyWorkspace O.view

-- | Tag current window to an empty workspace and view it. Do nothing if
-- all workspaces are in use.
tagToEmptyWorkspace :: X ()
tagToEmptyWorkspace = withEmptyWorkspace $ \w -> O.shift w >> O.view w
