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
-- Now you can jump to an empty workspace with mod-n. Mod-shift-n will
-- tag the current window to an empty workspace and view it.
--

module XMonadContrib.FindEmptyWorkspace (
    viewEmptyWorkspace, tagToEmptyWorkspace
  ) where

import Control.Monad.State
import qualified Data.Map as M

import XMonad
import Operations
import qualified StackSet as W

-- | Find the first empty workspace in a WindowSet. Returns Nothing if
-- all workspaces are in use.
findEmptyWorkspace :: WindowSet -> Maybe WorkspaceId
findEmptyWorkspace = findKey (([],[]) ==) . W.stacks

withEmptyWorkspace :: (WorkspaceId -> X ()) -> X ()
withEmptyWorkspace f = do
    ws <- gets windowset
    whenJust (findEmptyWorkspace ws) f

-- | Find and view an empty workspace. Do nothing if all workspaces are
-- in use.
viewEmptyWorkspace :: X ()
viewEmptyWorkspace = withEmptyWorkspace view

-- | Tag current window to an empty workspace and view it. Do nothing if
-- all workspaces are in use.
tagToEmptyWorkspace :: X ()
tagToEmptyWorkspace = withEmptyWorkspace $ \w -> tag w >> view w

-- Thanks to mauke on #haskell
findKey :: (a -> Bool) -> M.Map k a -> Maybe k
findKey f = M.foldWithKey (\k a -> mplus (if f a then Just k else Nothing)) Nothing
