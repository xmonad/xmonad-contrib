{-# OPTIONS_GHC -Wall #-}
module CycleRecentWS where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import XMonad.Actions.CycleRecentWS (unView)
import XMonad.StackSet (view, mapLayout)

import Instances
import Utils (tags)

spec :: Spec
spec = do
    prop "prop_unView" $ prop_unView

prop_unView :: T -> Property
prop_unView ss = conjoin
    [ counterexample (show t) (unView ss (state (view t ss)) === state ss)
    | t <- tags ss ]
  where
    state = mapLayout succ
