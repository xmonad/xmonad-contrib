{-# OPTIONS_GHC -Wall -Wno-orphans #-}
module IndependentScreens where

import Test.Hspec

import Data.List (sortOn)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W

spec :: Spec
spec = do
    context "currentScreenWS" $ do
        specify "Next stays on the current screen" $
            from 0 (cycleFrom Next currentScreenWS) `shouldReturn` "0_b"
        specify "Prev wraps around within the current screen" $
            from 0 (cycleFrom Prev currentScreenWS) `shouldReturn` "0_c"
        specify "follows the focused screen" $ do
            from 1 (cycleFrom Next currentScreenWS) `shouldReturn` "1_b"
            from 1 (cycleFrom Prev currentScreenWS) `shouldReturn` "1_c"
        specify "skips workspaces of other screens and unmarshalled tags" $ do
            -- for comparison, anyWS wraps around to the scratchpad
            from 0 (cycleFrom Prev anyWS) `shouldReturn` "NSP"
            from 0 (cycleFrom Prev currentScreenWS) `shouldReturn` "0_c"
        specify "can be combined with other WSTypes" $
            from 0 (cycleFrom Next (currentScreenWS :&: Not (WSIs (pure ((== "0_b") . W.tag))))) `shouldReturn` "0_c"
    context "onScreenWS" $ do
        specify "selects the workspaces of the given screen" $ do
            from 0 (cycleFrom Next (onScreenWS 1)) `shouldReturn` "1_a"
            from 0 (cycleFrom Next (onScreenWS 1 :&: hiddenWS)) `shouldReturn` "1_b"
        specify "does not confuse screens whose ids share a prefix" $
            from 0 (cycleFrom Prev (onScreenWS 1)) `shouldReturn` "1_c"

-- | Workspaces: three per screen on screens 0 and 1, one on screen 10, plus a scratchpad.
tags :: [WorkspaceId]
tags = withScreens 2 ["a", "b", "c"] ++ ["10_a", "NSP"]

-- | Find the workspace next to the one focused on screen @sc@.
from :: ScreenId -> X WorkspaceId -> IO WorkspaceId
from sc act = fst <$> runX conf st act
  where
    -- the actions under test only look at the 'windowset'
    conf = error "XConf is not needed"
    st = XState
        { windowset = focusScreen sc ws
        , mapped = mempty
        , waitingUnmap = mempty
        , dragging = Nothing
        , numberlockMask = 0
        , extensibleState = mempty
        }
    -- screen 0 shows 0_a, screen 1 shows 1_a
    ws = focusScreen 0 . W.view "1_a" . focusScreen 1 $
         W.new (Layout Full) tags [SD (Rectangle 0 0 1 1), SD (Rectangle 1 0 1 1)]

cycleFrom :: Direction1D -> WSType -> X WorkspaceId
cycleFrom dir t = findWorkspace (pure (sortOn W.tag)) dir t 1
