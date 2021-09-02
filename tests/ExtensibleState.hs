{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module ExtensibleState where

import Test.Hspec

import XMonad
import Data.Typeable
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M

data TestState = TestState Int deriving (Show, Read, Eq)
instance ExtensionClass TestState where
    initialValue = TestState 0

data TestPersistent = TestPersistent Int deriving (Show, Read, Eq)
instance ExtensionClass TestPersistent where
    initialValue = TestPersistent 0
    extensionType = PersistentExtension

spec :: Spec
spec = do
    describe "upgrade of non-persistent" $
        it "noop" $
            M.keys (XS.upgrade (undefined :: TestState) mempty) `shouldBe` mempty
    describe "upgrade of persistent" $ do
        describe "inserts initial value if not found" $ do
            let k = Right (typeOf (undefined :: TestPersistent))
            let m = XS.upgrade (undefined :: TestPersistent) mempty
            specify "keys" $ M.keys m `shouldBe` [k]
            specify "value" $ assertRightPersistent k m (TestPersistent 0)
        describe "noop if Right found" $ do
            let k = Right (typeOf (undefined :: TestPersistent))
            let m0 = M.singleton k (Right (PersistentExtension (TestPersistent 1)))
            let m = XS.upgrade (undefined :: TestPersistent) m0
            specify "keys" $ M.keys m `shouldBe` [k]
            specify "value" $ assertRightPersistent k m (TestPersistent 1)
        describe "deserialize" $ do
            let k0 = Left "ExtensibleState.TestPersistent"
            let m0 = M.singleton k0 (Left "TestPersistent 1")
            let k = Right (typeOf (undefined :: TestPersistent))
            let m = XS.upgrade (undefined :: TestPersistent) m0
            specify "keys" $ M.keys m `shouldBe` [k]
            specify "value" $ assertRightPersistent k m (TestPersistent 1)
        describe "upgrade from old representation and deserialize" $ do
            let k0 = Left "TestPersistent"
            let m0 = M.singleton k0 (Left "TestPersistent 1")
            let k = Right (typeOf (undefined :: TestPersistent))
            let m = XS.upgrade (undefined :: TestPersistent) m0
            specify "keys" $ M.keys m `shouldBe` [k]
            specify "value" $ assertRightPersistent k m (TestPersistent 1)

assertRightPersistent :: (Ord k, Typeable v, Show v, Eq v)
                      => k -> M.Map k (Either String StateExtension) -> v -> Expectation
assertRightPersistent k m v = case k `M.lookup` m of
    Just (Right (PersistentExtension (cast -> Just x))) -> x `shouldBe` v
    _ -> expectationFailure "unexpected"
