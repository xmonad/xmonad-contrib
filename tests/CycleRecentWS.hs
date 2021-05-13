{-# OPTIONS_GHC -Wall #-}
module CycleRecentWS where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import XMonad.Actions.CycleRecentWS (unView)
import XMonad.StackSet (view)

import Instances
import Utils (tags)

spec :: Spec
spec = do
    prop "prop_unView" $ prop_unView

prop_unView :: T -> Property
prop_unView ss = conjoin
    [ counterexample (show t) (unView ss (view t ss) === ss) | t <- tags ss ]
