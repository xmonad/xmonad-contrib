module GridSelect where

import Test.Hspec
import Test.Hspec.QuickCheck

import XMonad.Actions.GridSelect

spec :: Spec
spec = do
  prop "prop_stringToRatio_valuesInRange"  prop_stringToRatio_valuesInRange

prop_stringToRatio_valuesInRange :: String -> Bool
prop_stringToRatio_valuesInRange s =
  let r = stringToRatio s
  in r >= 0 && r <= 1
