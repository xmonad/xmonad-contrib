module XMonadContrib.RotView ( rotView ) where

-- Provides bindings to cycle through non-empty workspaces.

-- To use:
-- import XMonadContrib.RotView

--    , ((modMask .|. shiftMask, xK_Right), rotView True)
--    , ((modMask .|. shiftMask, xK_Left), rotView False)

import qualified Data.Map as M
import Control.Monad.State

import Operations ( view )
import XMonad ( X, WorkspaceId, workspace, whenJust )
import StackSet ( StackSet )
import Data.Maybe ( listToMaybe )
import qualified StackSet as W ( stacks, current, visibleWorkspaces, index )

rotView :: Bool -> X ()
rotView b = do ws <- gets windowset
               let m = W.current ws
                   vis = W.visibleWorkspaces ws
                   allws = if b then allWorkspaces ws else reverse $ allWorkspaces ws
                   pivoted = uncurry (flip (++)) . span (/=m) $ allws
                   interesting i = not (i `elem` vis) && not (isEmpty i ws)
                   nextws = listToMaybe . filter interesting $ pivoted
               whenJust nextws view

-- | A list of all the workspaces.
allWorkspaces :: StackSet WorkspaceId j a -> [WorkspaceId]
allWorkspaces = M.keys . W.stacks

isEmpty :: WorkspaceId -> StackSet WorkspaceId j a -> Bool
isEmpty i = maybe True null . W.index i
