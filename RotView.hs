module XMonadContrib.RotView ( rotView ) where

-- Provides bindings to cycle through non-empty workspaces.

-- To use:
-- import XMonadContrib.RotView

--    , ((modMask .|. shiftMask, xK_Right), rotView True)
--    , ((modMask .|. shiftMask, xK_Left), rotView False)

import Control.Monad.State ( gets )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe )

import XMonad
import StackSet hiding (filter)
import qualified Operations as O

rotView :: Bool -> X ()
rotView b = do
    ws <- gets windowset
    let m = tag . workspace . current $ ws
        sortWs = sortBy (\x y -> compare (tag x) (tag y))
        pivoted = uncurry (flip (++)) . span ((< m) . tag) . sortWs . hidden $ ws
        nextws = listToMaybe . filter (not.isEmpty) . (if b then id else reverse) $ pivoted
    whenJust nextws (O.view . tag)

isEmpty :: Workspace i a -> Bool
isEmpty ws = case stack ws of
                Empty     -> True
                _         -> False
