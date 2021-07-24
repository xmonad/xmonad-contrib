{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module NoBorders where

import Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Map as M

import XMonad hiding (Screen)
import qualified XMonad.Layout.NoBorders as NB
import XMonad.Prelude
import XMonad.StackSet

spec :: Spec
spec = do
    describe "dualhead, fullscreen float on each" $ do
        let s1 = differentiate [1]
        let s2 = differentiate [2]
        let floats = [(1, rrFull), (2, rrFull)]
        let ws = wsDualHead s1 s2 floats
        context "Ambiguity(Never)" $ do
            let amb = NB.Never
            it "removes border on current screen" $ do
                NB.hiddens amb ws r1 s1 [] `shouldBe` [1]
                NB.hiddens amb ws r3 s1 [] `shouldBe` [1]
            it "removes border on visible screen" $ do
                NB.hiddens amb ws r2 s2 [] `shouldBe` [2]
                NB.hiddens amb ws r4 s2 [] `shouldBe` [2]
        context "Ambiguity(OnlyScreenFloat)" $ do
            let amb = NB.OnlyScreenFloat
            it "removes border on current screen" $ do
                NB.hiddens amb ws r1 s1 [] `shouldBe` [1]
                NB.hiddens amb ws r3 s1 [] `shouldBe` [1]
            it "removes border on visible screen" $ do
                NB.hiddens amb ws r2 s2 [] `shouldBe` [2]
                NB.hiddens amb ws r4 s2 [] `shouldBe` [2]
        context "Ambiguity(OnlyLayoutFloat)" $ do
            let amb = NB.OnlyLayoutFloat
            it "removes border on current screen" $ do
                NB.hiddens amb ws r1 s1 [] `shouldBe` [1]
            it "removes border on visible screen" $ do
                NB.hiddens amb ws r2 s2 [] `shouldBe` [2]
        prop "prop_OnlyFloat" prop_OnlyFloat

-- | All floating windows should be borderless.
prop_OnlyFloat
    :: [Window]       -- ^ Windows on the first monitor
    -> [Window]       -- ^ Windows on the second monitor
    -> [RationalRect] -- ^ Floating window rectangles
    -> Bool           -- ^ Whether to consider focused or visible screen
    -> Bool
prop_OnlyFloat (nub -> w1) (nub -> w2) frs b
     = sort (w `intersect` map fst floats)
    == sort (NB.hiddens NB.OnlyFloat ws r (differentiate w) [])
  where
    (w, w', r) = if b then (w1, w2, r1) else (w2, w1, r2)
    ws         = wsDualHead (differentiate w1) (differentiate w2) floats
    floats     = zip (interleave w w') frs

    interleave :: [a] -> [a] -> [a]
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave []       ys       = ys
    interleave xs       []       = xs

-- +------+------+
-- |  r1  |  r2  |
-- |      |      |
-- |+----+|+----+|
-- || r3 ||| r4 ||
-- |+----+|+----+|
-- +------+------+
r1, r2, r3, r4 :: Rectangle
r1 = Rectangle   0  0 100 100
r2 = Rectangle 100  0 100 100
r3 = Rectangle  10 10  80  80
r4 = Rectangle 110 10  80  80

rrFull :: RationalRect
rrFull = RationalRect 0 0 1 1

-- | Current screen @r1@ with window stack @w1@,
-- visible screen @r2@ with ws @w2@,
-- no hidden screens, maybe some floats.
wsDualHead :: Maybe (Stack Window) -> Maybe (Stack Window)
           -> [(Window, RationalRect)] -> WindowSet
wsDualHead w1 w2 f = StackSet{..}
    where
        current = mkScreen 1 r1 w1; visible = [mkScreen 2 r2 w2]; hidden = []
        floating = M.fromList f

mkScreen :: ScreenId -> Rectangle -> Maybe (Stack Window)
         -> Screen WorkspaceId l Window ScreenId ScreenDetail
mkScreen i r s = Screen{ workspace = w, screen = i, screenDetail = sd }
    where
        w = Workspace{ tag = show i, layout = undefined, stack = s }
        sd = SD{ screenRect = r }
