-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.FindEmptyWorkspace
-- Description :  Find an empty workspace.
-- Copyright   :  (c) Miikka Koskinen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  arcatan@kapsi.fi
-- Stability   :  stable
-- Portability :  unportable
--
-- Find an empty workspace.
--
-----------------------------------------------------------------------------

module XMonad.Actions.FindEmptyWorkspace (
    -- * Usage
    -- $usage
    viewEmptyWorkspace, tagToEmptyWorkspace, sendToEmptyWorkspace
  ) where

import XMonad.Prelude
import XMonad
import XMonad.StackSet

-- $usage
--
-- To use, import this module into your @~\/.xmonad\/xmonad.hs@:
--
-- >   import XMonad.Actions.FindEmptyWorkspace
--
-- and add the desired keybindings, for example:
--
--  >   , ((modm,                xK_m    ), viewEmptyWorkspace)
--  >   , ((modm .|. shiftMask,  xK_m    ), tagToEmptyWorkspace)
--
-- Now you can jump to an empty workspace with @mod-m@. @Mod-shift-m@
-- will tag the current window to an empty workspace and view it.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Find the first hidden empty workspace in a StackSet. Returns
-- Nothing if all workspaces are in use. Function searches currently
-- focused workspace, other visible workspaces (when in Xinerama) and
-- hidden workspaces in this order.
findEmptyWorkspace :: StackSet i l a s sd -> Maybe (Workspace i l a)
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
viewEmptyWorkspace = withEmptyWorkspace (windows . view)

-- | Tag current window to an empty workspace and view it. Do nothing if
-- all workspaces are in use.
tagToEmptyWorkspace :: X ()
tagToEmptyWorkspace = withEmptyWorkspace $ \w -> windows $ view w . shift w

-- | Send current window to an empty workspace. Do nothing if
-- all workspaces are in use.
sendToEmptyWorkspace :: X ()
sendToEmptyWorkspace = withEmptyWorkspace $ \w -> windows $ shift w
