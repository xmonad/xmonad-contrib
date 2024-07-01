{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WindowNavigation where

import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor.Identity

import XMonad
import XMonad.Util.Types (Direction2D(..))
import XMonad.Actions.WindowNavigation (goPure, swapPure, WNState)
import qualified XMonad.StackSet as W

spec :: Spec
spec = do
  it "two-window adjacent go right (empty state)" $ do
    -- Simplest case - just move the focus once.
    -- ┌─────┬──────┐
    -- │ 1 ──┼─►  2 │
    -- └─────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1, 2], windowRect)
    runNav R M.empty (mkws 1 [] [2])
      `shouldBe` (mkstate 960 640, mkws 2 [1] [])

  it "two-window adjacent go right (populated state)" $ do
    -- Like the previous test, but this time internal stat is already populated with a position.
    -- ┌─────┬──────┐
    -- │ 1 ──┼─►  2 │
    -- └─────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1, 2], windowRect)
    runNav R (mkstate 100 100) (mkws 1 [] [2])
      `shouldBe` (mkstate 960 100, mkws 2 [1] [])

  it "two-window adjacent go right (incorrectly-populated state)" $ do
    -- This time we set the position incorrectly, testing if it will be reset to the center of focused window.
    -- ┌─────┬──────┐
    -- │ 1 ──┼─►  2 │
    -- └─────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1, 2], windowRect)
    runNav R (mkstate 1000 100) (mkws 1 [] [2])
      `shouldBe` (mkstate 960 640, mkws 2 [1] [])

  it "swap windows" $ do
    -- Swap windows around.
    -- ┌─────┬──────┐
    -- │ 1 ◄─┼─►  2 │
    -- └─────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          ]
    runIdentity (swapPure R (M.empty, mkws 1 [] [2], S.fromList [1, 2], windowRect))
      `shouldBe` (mkstate 960 640, mkws 1 [2] [])

  it "tall layout, go up" $ do
    -- ┌─────┬─────┐
    -- │     │ 2 ▲ │
    -- │  1  ├───┼─┤
    -- │     │ 3 │ │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 640)
          , (3, Rectangle 960 640 960 640)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1, 2, 3], windowRect)
    runNav U M.empty (mkws 3 [] [1, 2])
      `shouldBe` (mkstate 1440 639, mkws 2 [1, 3] [])

  it "tall layout, go down" $ do
    -- ┌─────┬─────┐
    -- │     │ 2   │
    -- │     ├─────┤
    -- │  1  │ 3 │ │
    -- │     ├───┼─┤
    -- │     │ 4 ▼ │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    runNav D M.empty (mkws 3 [] [1, 2, 4])
      `shouldBe` (mkstate 1440 800, mkws 4 [2, 1, 3] [])

  it "tall layout, go left" $ do
    -- ┌─────┬─────┐
    -- │   ◄─┼── 2 │
    -- │     ├─────┤
    -- │ 1   │   3 │
    -- │     ├─────┤
    -- │     │   4 │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    runNav L M.empty (mkws 2 [] [1, 3, 4])
      `shouldBe` (mkstate 959 200, mkws 1 [2] [3, 4])

  it "tall layout, go left and then right (window 2)" $ do
    -- ┌─────┬─────┐
    -- │   ◄─┼── 2 │
    -- │   ──┼─►   │
    -- │     ├─────┤
    -- │ 1   │   3 │
    -- │     ├─────┤
    -- │     │   4 │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav L M.empty (mkws 2 [] [1, 3, 4])
    (st2, ws2) `shouldBe` (mkstate 959 200, mkws 1 [2] [3, 4])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 200, mkws 2 [] [1, 3, 4])

  it "tall layout, go left and then right (window 3)" $ do
    -- ┌─────┬─────┐
    -- │     │   2 │
    -- │     ├─────┤
    -- │ 1 ◄─┼── 3 │
    -- │   ──┼─►   │
    -- │     ├─────┤
    -- │     │   4 │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav L M.empty (mkws 3 [] [1, 2, 4])
    (st2, ws2) `shouldBe` (mkstate 959 600, mkws 1 [3] [2, 4])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 600, mkws 3 [] [1, 2, 4])

  it "tall layout, go left and then right (window 4)" $ do
    -- ┌─────┬─────┐
    -- │     │   2 │
    -- │     ├─────┤
    -- │ 1   │   3 │
    -- │     ├─────┤
    -- │   ◄─┼── 4 │
    -- │   ──┼─►   │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav L M.empty (mkws 4 [] [1, 2, 3])
    (st2, ws2) `shouldBe` (mkstate 959 1040, mkws 1 [4] [2, 3])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 1040, mkws 4 [] [1, 2, 3])

  it "grid layout, go in a circle" $ do
    -- ┌─────┬─────┐
    -- │ 1 ──┼─► 2 │
    -- │     │     │
    -- │ ▲   │   │ │
    -- ├─┼───┼───┼─┤
    -- │ │   │   ▼ │
    -- │     │     │
    -- │ 3 ◄─┼── 4 │
    -- └─────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 640)
          , (2, Rectangle 960 0 960 640)
          , (3, Rectangle 0 640 960 640)
          , (4, Rectangle 960 640 960 640)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav R M.empty (mkws 1 [] [2, 3, 4])
    (st2, ws2) `shouldBe` (mkstate 960 320, mkws 2 [1] [3, 4])
    let (st3, ws3) = runNav D st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 640, mkws 4 [3, 2, 1] [])
    let (st4, ws4) = runNav L st3 ws3
    (st4, ws4) `shouldBe` (mkstate 959 640, mkws 3 [2, 1] [4])
    let (st5, ws5) = runNav U st4 ws4
    (st5, ws5) `shouldBe` (mkstate 959 639, mkws 1 [] [2, 3, 4])

  it "ignore window that fully overlaps the current window in parallel direction when pos is outside it" $ do
    -- ┌─────┬──────┬──────┐
    -- │ ┌───┴──────┴────┐ │
    -- │ │   |  4   |    │ │
    -- │ └───┬──────┬────┘ │
    -- │  1  │  2 ──┼─► 3  │
    -- └─────┴──────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 600 1280)
          , (2, Rectangle 600 0 600 1280)
          , (3, Rectangle 1200 0 720 1280)
          , (4, Rectangle 200 200 1520 400)
          ]
    runIdentity (goPure R (mkstate 900 900, mkws 2 [] [1, 3, 4], S.fromList [1..4], windowRect))
      `shouldBe` (mkstate 1200 900, mkws 3 [1,2] [4])

  it "go to window that fully overlaps the current window in parallel direction when pos is inside it" $ do
    -- ┌─────────────────┐
    -- │     ┌──────┐    │
    -- │  1  │      │    │
    -- ├─────┤------├────┤
    -- │     │      │    │
    -- │  2  │  4 ──┼─►  │
    -- │     │      │    │
    -- ├─────┤------├────┤
    -- │  3  │      │    │
    -- │     └──────┘    │
    -- └─────────────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 1920 400)
          , (2, Rectangle 0 400 1920 400)
          , (3, Rectangle 0 800 1920 480)
          , (4, Rectangle 800 200 400 880)
          ]
    runIdentity (goPure R (mkstate 1000 600, mkws 4 [] [1, 2, 3], S.fromList [1..4], windowRect))
      `shouldBe` (mkstate 1200 600, mkws 2 [1,4] [3])

  it "go from inner window to outer" $ do
    -- ┌───────────────┐
    -- │      ┌──────┐ │
    -- │  1 ◄─┼── 2  │ │
    -- │      └──────┘ │
    -- └───────────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 1920 1280)
          , (2, Rectangle 600 600 600 600)
          ]
    runIdentity (goPure L (M.empty, mkws 2 [] [1], S.fromList [1, 2], windowRect))
      `shouldBe` (mkstate 599 900, mkws 1 [2] [])

  it "if there are multiple outer windows, go to the smaller one" $ do
    -- ┌────────────────────────┐
    -- │  ┌───────────────┐     │
    -- │  │      ┌──────┐ │     │
    -- │  │  2 ◄─┼── 3  │ │  1  │
    -- │  │      └──────┘ │     │
    -- │  └───────────────┘     │
    -- └────────────────────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 1920 1280)
          , (2, Rectangle 200 200 1520 880)
          , (3, Rectangle 400 400 400 400)
          ]
    runIdentity (goPure L (M.empty, mkws 3 [] [1, 2], S.fromList [1..3], windowRect))
      `shouldBe` (mkstate 399 600, mkws 2 [1, 3] [])

  it "two tiled and one floating, floating fully inside" $ do
    -- ┌───────────────────┬─────┐
    -- │   ┌───────┐       │     │
    -- │ ──┼─►   ──┼─►   ──┼─►   │
    -- │   │   3   │   1   │   2 │
    -- │   │     ◄─┼──   ◄─┼──   │
    -- │   └───────┘       │     │
    -- └───────────────────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          , (3, Rectangle 400 400 400 400)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..3], windowRect)
    let (st2, ws2) = runNav R (mkstate 100 100) (mkws 1 [] [2, 3])
    (st2, ws2) `shouldBe` (mkstate 400 400, mkws 3 [2, 1] [])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 800 400, mkws 1 [] [2, 3])
    let (st4, ws4) = runNav R st3 ws3
    (st4, ws4) `shouldBe` (mkstate 960 400, mkws 2 [1] [3])
    let (st5, ws5) = runNav L st4 ws4
    (st5, ws5) `shouldBe` (mkstate 959 400, mkws 1 [] [2, 3])
    let (st6, ws6) = runNav L st5 ws5
    (st6, ws6) `shouldBe` (mkstate 799 400, mkws 3 [2, 1] [])

  it "two floating windows inside one big tiled one" $ do
    -- ┌─────────┐
    -- │    │    │
    -- │ ┌──┼──┐ │
    -- │ │  ▼  │ │
    -- │ │  3  │ │
    -- │ └──┼──┘ │
    -- │    ▼    │
    -- │    1    │
    -- │ ┌──┼──┐ │
    -- │ │  ▼  │ │
    -- │ │  4  │ │
    -- │ └──┼──┘ │
    -- │    ▼    │
    -- ├────┼────┤
    -- │    ▼    │
    -- │    2    │
    -- └─────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 1920 640)
          , (2, Rectangle 0 640 1920 640)
          , (3, Rectangle 200 200 100 100)
          , (4, Rectangle 1000 400 100 100)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav D (mkstate 1000 250) (mkws 1 [] [2, 3, 4])
    (st2, ws2) `shouldBe` (mkstate 299 250, mkws 3 [2, 1] [4])
    let (st3, ws3) = runNav D st2 ws2
    (st3, ws3) `shouldBe` (mkstate 299 300, mkws 1 [] [2, 3, 4])
    let (st4, ws4) = runNav D st3 ws3
    (st4, ws4) `shouldBe` (mkstate 1000 400, mkws 4 [3, 2, 1] [])
    let (st5, ws5) = runNav D st4 ws4
    (st5, ws5) `shouldBe` (mkstate 1000 500, mkws 1 [] [2, 3, 4])
    let (st6, ws6) = runNav D st5 ws5
    (st6, ws6) `shouldBe` (mkstate 1000 640, mkws 2 [1] [3, 4])

  it "floating window between two tiled ones" $ do
    -- ┌───────┬────────┐
    -- │ 1 ┌───┴───┐  2 │
    -- │ ──┼─► 3 ──┼─►  │
    -- │   └───┬───┘    │
    -- └───────┴────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          , (3, Rectangle 860 540 200 200)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..3], windowRect)
    let (st2, ws2) = runNav R M.empty (mkws 1 [] [2, 3])
    (st2, ws2) `shouldBe` (mkstate 860 640, mkws 3 [2, 1] [])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 640, mkws 2 [1] [3])

  it "floating window overlapping four tiled ones" $ do
    -- ┌───────┬───────┐
    -- │   ┌───┴───┐   │
    -- │ 1 │       │ 2 │
    -- ├───┤       ├───┤
    -- │ ──┼─► 5 ──┼─► │
    -- │ 3 └───┬───┘ 4 │
    -- └───────┴───────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 640)
          , (2, Rectangle 960 0 960 640)
          , (3, Rectangle 0 640 960 640)
          , (4, Rectangle 960 640 960 640)
          , (5, Rectangle 760 440 400 400)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..5], windowRect)
    let (st2, ws2) = runNav R (mkstate 480 640) (mkws 3 [] [1, 2, 4, 5])
    (st2, ws2) `shouldBe` (mkstate 760 640, mkws 5 [4, 2, 1, 3] [])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 960 640, mkws 4 [2, 1, 3] [5])

  it "sequential inner floating windows" $ do
    -- ┌───────────────────────────────────┬──────┐
    -- │   ┌───────┐                       │      │
    -- │   │       │       ┌───────┐       │      │
    -- │ ──┼─► 3 ──┼─► 1 ──┼─► 4 ──┼─►   ──┼─► 2  │
    -- │ ◄─┼──   ◄─┼──   ◄─┼──   ◄─┼──   ◄─┼──    │
    -- │   └───────┘       │       │       │      │
    -- │                   └───────┘       │      │
    -- └───────────────────────────────────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          , (3, Rectangle 200 200 200 200)
          , (4, Rectangle 600 600 200 200)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav R (mkstate 100 100) (mkws 1 [] [2, 3, 4])
    (st2, ws2) `shouldBe` (mkstate 200 200, mkws 3 [2,1] [4])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 400 200, mkws 1 [] [2, 3, 4])
    let (st4, ws4) = runNav R st3 ws3
    (st4, ws4) `shouldBe` (mkstate 600 600, mkws 4 [3, 2, 1] [])
    let (st5, ws5) = runNav R st4 ws4
    (st5, ws5) `shouldBe` (mkstate 800 600, mkws 1 [] [2, 3, 4])
    let (st6, ws6) = runNav R st5 ws5
    (st6, ws6) `shouldBe` (mkstate 960 600, mkws 2 [1] [3, 4])
    let (st7, ws7) = runNav L st6 ws6
    (st7, ws7) `shouldBe` (mkstate 959 600, mkws 1 [] [2, 3, 4])
    let (st8, ws8) = runNav L st7 ws7
    (st8, ws8) `shouldBe` (mkstate 799 600, mkws 4 [3, 2, 1] [])
    let (st9, ws9) = runNav L st8 ws8
    (st9, ws9) `shouldBe` (mkstate 599 600, mkws 1 [] [2, 3, 4])
    let (st10, ws10) = runNav L st9 ws9
    (st10, ws10) `shouldBe` (mkstate 399 399, mkws 3 [2, 1] [4])
    let (st11, ws11) = runNav L st10 ws10
    (st11, ws11) `shouldBe` (mkstate 199 399, mkws 1 [] [2, 3, 4])

  it "overlapping inner floating windows" $ do
    -- ┌─────────────────────┬──────┐
    -- │ ┌─────────┐         │      │
    -- │ │  3 ┌────┴─┐       │      │
    -- │ │  ──┼─►  ──┼─► 1 ──┼─►  2 │
    -- │ │  ◄─┼──  ◄─┼──   ◄─┼──    │
    -- │ │    │  4   │       │      │
    -- │ └────┤      │       │      │
    -- │      └──────┘       │      │
    -- └─────────────────────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          , (3, Rectangle 200 200 400 400)
          , (4, Rectangle 300 300 400 400)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..4], windowRect)
    let (st2, ws2) = runNav R M.empty (mkws 3 [] [1, 2, 4])
    (st2, ws2) `shouldBe` (mkstate 400 400, mkws 4 [2, 1, 3] [])
    let (st3, ws3) = runNav R st2 ws2
    (st3, ws3) `shouldBe` (mkstate 700 400, mkws 1 [3] [2, 4])
    let (st4, ws4) = runNav R st3 ws3
    (st4, ws4) `shouldBe` (mkstate 960 400, mkws 2 [1, 3] [4])
    let (st5, ws5) = runNav L st4 ws4
    (st5, ws5) `shouldBe` (mkstate 959 400, mkws 1 [3] [2, 4])
    let (st6, ws6) = runNav L st5 ws5
    (st6, ws6) `shouldBe` (mkstate 699 400, mkws 4 [2, 1, 3] [])
    let (st7, ws7) = runNav L st6 ws6
    (st7, ws7) `shouldBe` (mkstate 599 400, mkws 3 [] [1, 2, 4])

  it "bounce back from the wall to the floating window" $ do
    -- ┌────────────────┬─────┐
    -- │  1   ┌──────┐  │     │
    -- │  ┌───┼─►  3 │  │  2  │
    -- │  └── │      │  │     │
    -- │      └──────┘  │     │
    -- └────────────────┴─────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 1280)
          , (3, Rectangle 400 400 200 200)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..3], windowRect)
    runNav L (mkstate 100 640) (mkws 1 [] [2, 3])
      `shouldBe` (mkstate 400 599, mkws 3 [2, 1] [])

  it "jump between screens" $ do
    -- ┌─────┬──────┐  ┌────────┐
    -- │     │  2   │  │    5   │
    -- │     ├──────┤  ├────────┤
    -- │  1  │  3 ──┼──┼─►  6   │
    -- │     ├──────┤  └────────┘
    -- │     │  4   │
    -- └─────┴──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          , (5, Rectangle 1920 0 1280 384)
          , (6, Rectangle 1920 384 1280 384)
          ]
        initWindowSet =
          W.StackSet
          { W.current =
              W.Screen
              { W.workspace =
                  W.Workspace
                  { W.tag = "A"
                  , W.layout = Layout NullLayout
                  , W.stack = Just $ W.Stack { W.focus = 3, W.up = [], W.down = [1, 2, 4] }
                  }
              , W.screen = 1
              , W.screenDetail = SD { screenRect = Rectangle 0 0 1920 1280 }
              }
          , W.visible =
              [ W.Screen
                { W.workspace =
                    W.Workspace
                    { W.tag = "B"
                    , W.layout = Layout NullLayout
                    , W.stack = Just $ W.Stack { W.focus = 5, W.up = [], W.down = [6] }
                    }
                , W.screen = 2
                , W.screenDetail = SD { screenRect = Rectangle 1920 0 1280 768 }
                }
              ]
          , W.hidden = []
          , W.floating = M.empty
          }
        expectedWindowSet =
          W.StackSet
          { W.current =
              W.Screen
              { W.workspace =
                  W.Workspace
                  { W.tag = "B"
                  , W.layout = Layout NullLayout
                  , W.stack = Just $ W.Stack { W.focus = 6, W.up = [5], W.down = [] }
                  }
              , W.screen = 2
              , W.screenDetail = SD { screenRect = Rectangle 1920 0 1280 768 }
              }
          , W.visible =
              [ W.Screen
                { W.workspace =
                    W.Workspace
                    { W.tag = "A"
                    , W.layout = Layout NullLayout
                    , W.stack = Just $ W.Stack { W.focus = 3, W.up = [], W.down = [1, 2, 4] }
                    }
                , W.screen = 1
                , W.screenDetail = SD { screenRect = Rectangle 0 0 1920 1280 }
                }
              ]
          , W.hidden = []
          , W.floating = M.empty
          }

    runIdentity (goPure R (M.empty, initWindowSet, S.fromList [1..6], windowRect))
      `shouldBe` (M.fromList [("B", Point 1920 600)], expectedWindowSet)

  it "floating window overlapping fully in the orthogonal direction" $ do
    -- ┌─────┬──────────────────┐
    -- │     │      ┌───────┐   │
    -- │     │    2 │       │   │
    -- │     ├──────┤-------├───┤
    -- │  1  │    3 │       │ 3 │
    -- │   ◄─┼──  ◄─┼── 5 ◄─┼── │
    -- │     ├──────┤-------├───┤
    -- │     │    4 │       │   │
    -- │     │      └───────┘   │
    -- └─────┴──────────────────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 960 0 960 400)
          , (3, Rectangle 960 400 960 400)
          , (4, Rectangle 960 800 960 480)
          , (5, Rectangle 1360 200 200 800)
          ]
        runNav dir st ws = runIdentity $ goPure dir (st, ws, S.fromList [1..5], windowRect)
    let (st2, ws2) = runNav L (mkstate 1800 600) (mkws 3 [] [1, 2, 4, 5])
    (st2, ws2) `shouldBe` (mkstate 1559 600, mkws 5 [4, 2, 1, 3] [])
    let (st3, ws3) = runNav L st2 ws2
    (st3, ws3) `shouldBe` (mkstate 1359 600, mkws 3 [] [1, 2, 4, 5])
    let (st4, ws4) = runNav L st3 ws3
    (st4, ws4) `shouldBe` (mkstate 959 600, mkws 1 [3] [2, 4, 5])

  it "navigation to free-floating windows on the same screen" $ do
    -- ┌──────┐
    -- │      │  ┌──────┐
    -- │      │  │      │
    -- │    ──┼──┼─►  2 │
    -- │      │  │      │
    -- │   1  │  └──────┘
    -- │      │
    -- │      │
    -- └──────┘
    let windowRect w =
          Identity $ M.lookup w $ M.fromList
          [ (1, Rectangle 0 0 960 1280)
          , (2, Rectangle 1200 400 400 400)
          ]
    runIdentity (goPure R (M.empty, mkws 1 [] [2], S.fromList [1, 2], windowRect))
      `shouldBe` (mkstate 1200 640, mkws 2 [1] [])

  it "switch between windows in Full layout" $ do
    let windowRect w = Identity $ M.lookup w $ M.fromList [(1, Rectangle 0 0 1920 1280)]
    runIdentity (goPure D (M.empty, mkws 1 [] [2, 3], S.fromList [1], windowRect))
      `shouldBe` (M.empty, mkws 2 [1] [3])

data NullLayout a = NullLayout deriving (Show, Read, Eq)
instance LayoutClass NullLayout a

-- to make WindowSets comparable
instance Eq (Layout w) where
  (==) a b = show a == show b
  (/=) a b = show a /= show b

-- make a state with a position for a single workspace
mkstate :: Position -> Position -> WNState
mkstate px py = M.fromList [("A", Point px py)]

-- make a single-workspace WindowSet
mkws :: Window -> [Window] -> [Window] -> WindowSet
mkws focusedWindow upWindows downWindows = W.StackSet
  { W.current = W.Screen
    { W.workspace = W.Workspace
      { W.tag = "A"
      , W.layout = Layout NullLayout
      , W.stack = Just $ W.Stack { W.focus = focusedWindow, W.up = upWindows, W.down = downWindows }
      }
    , W.screen = 1
    , W.screenDetail = SD { screenRect = Rectangle 0 0 1920 1280 }
    }
  , W.visible = []
  , W.hidden = []
  , W.floating = M.empty
  }
