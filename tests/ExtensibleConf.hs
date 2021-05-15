{-# OPTIONS_GHC -Wall #-}
module ExtensibleConf where

import Test.Hspec

import XMonad
import qualified XMonad.Util.ExtensibleConf as XC

spec :: Spec
spec = do
    specify "lookup" $
        XC.lookup def `shouldBe` (Nothing :: Maybe ())
    specify "lookup . add" $
        XC.lookup (XC.add "a" def) `shouldBe` Just "a"
    specify "lookup . add . add" $
        XC.lookup (XC.add "b" (XC.add "a" def)) `shouldBe` Just "ab"
    specify "lookup @String . add @String . add @[Int]" $
        XC.lookup (XC.add "a" (XC.add [1 :: Int] def)) `shouldBe` Just "a"
    specify "lookup @[Int] . add @String . add @[Int]" $
        XC.lookup (XC.add "a" (XC.add [1 :: Int] def)) `shouldBe` Just [1 :: Int]
    specify "lookup @() . add @String . add @[Int]" $
        XC.lookup (XC.add "a" (XC.add [1 :: Int] def)) `shouldBe` (Nothing :: Maybe ())

    specify "once" $
        borderWidth (XC.once "a" incBorderWidth def) `shouldBe` succ (borderWidth def)
    specify "once . once" $
        borderWidth (XC.once "b" incBorderWidth (XC.once "a" incBorderWidth def))
            `shouldBe` succ (borderWidth def)

incBorderWidth :: XConfig l -> XConfig l
incBorderWidth c = c{ borderWidth = succ (borderWidth c) }
