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

    specify "once" $ do
        let c = XC.once incBorderWidth "a" def
        borderWidth c `shouldBe` succ (borderWidth def)
        XC.lookup c `shouldBe` Just "a"
    specify "once . once" $ do
        let c = XC.once incBorderWidth "b" (XC.once incBorderWidth "a" def)
        borderWidth c `shouldBe` succ (borderWidth def)
        XC.lookup c `shouldBe` Just "ab"

    specify "modifyDef" $ do
        let c = XC.modifyDef (<> "a") def
        XC.lookup c `shouldBe` Just "a"
    specify "modifyDef . modifyDef" $ do
        let c = XC.modifyDef (<> "b") (XC.modifyDef (<> "a") def)
        XC.lookup c `shouldBe` Just "ab"

incBorderWidth :: XConfig l -> XConfig l
incBorderWidth c = c{ borderWidth = succ (borderWidth c) }
