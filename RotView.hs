module XMonadContrib.RotView ( rotView ) where

-- Provides bindings to cycle through non-empty workspaces.

-- To use:
-- import XMonadContrib.RotView

--    , ((modMask .|. shiftMask, xK_Right), rotView True)
--    , ((modMask .|. shiftMask, xK_Left), rotView False)

import qualified Data.Map as M
import Control.Monad.State

import Operations ( view )
import XMonad ( X, WorkspaceId, workspace )
import StackSet ( StackSet, focus )
import qualified StackSet as W ( current )

rotView :: Bool -> X ()
rotView b = do ws <- gets workspace
               let m = W.current ws
                   allws = if b then allWorkspaces ws else reverse $ allWorkspaces ws
                   n1 = safehead allws m
                   rot (f:fs) | f == m = safehead fs n1
                              | otherwise = rot fs
                   rot [] = n1
                   safehead fs f = case fs of { [] -> f; f':_ -> f'; }
               view (rot allws)

-- | A list of all the workspaces.
allWorkspaces :: StackSet WorkspaceId j a -> [WorkspaceId]
allWorkspaces = M.keys . focus
