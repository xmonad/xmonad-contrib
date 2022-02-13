{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module EZConfig (spec) where

import Control.Arrow (first, (>>>))
import Data.Coerce
import Foreign.C.Types (CUInt(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import XMonad
import XMonad.Prelude
import XMonad.Util.EZConfig
import XMonad.Util.Parser

spec :: Spec
spec = do
  prop "prop_decodePreservation" prop_decodePreservation
  prop "prop_encodePreservation" prop_encodePreservation

  context "parseKey" $ do
    let prepare = unzip . map (first surround)
        testParseKey (ns, ks) = traverse (runParser parseKey) ns `shouldBe` Just ks
    it "parses all regular keys"    $ testParseKey (unzip   regularKeys   )
    it "parses all function keys"   $ testParseKey (prepare functionKeys  )
    it "parses all special keys"    $ testParseKey (prepare specialKeys   )
    it "parses all multimedia keys" $ testParseKey (prepare multimediaKeys)
  context "parseModifier" $ do
    it "parses all combinations of modifiers" $
      nub . map sort <$> traverse (runParser (many $ parseModifier def))
                                     modifiers
        `shouldBe` Just [[ shiftMask, controlMask
                         , mod1Mask, mod1Mask      -- def M and M1
                         , mod2Mask, mod3Mask, mod4Mask, mod5Mask
                         ]]

  -- Checking for regressions
  describe "readKeySequence" $
    it "Fails on the non-existent key M-10" $
      readKeySequence def "M-10" `shouldBe` Nothing

-- | Parsing preserves all info that printing does.
prop_encodePreservation :: KeyString -> Property
prop_encodePreservation (coerce -> s) = parse s === (parse . pp =<< parse s)
 where parse = runParser (parseKeySequence def)
       pp    = unwords . map keyToString

-- | Printing preserves all info that parsing does.
prop_decodePreservation :: NonEmptyList (AKeyMask, AKeySym) -> Property
prop_decodePreservation (getNonEmpty >>> coerce -> xs) =
  Just (pp xs) === (fmap pp . parse $ pp xs)
 where parse = runParser (parseKeySequence def)
       pp    = unwords . map keyToString

-- | QuickCheck can handle the 8! combinations just fine.
modifiers :: [String]
modifiers = map concat $ permutations mods

mods :: [String]
mods = ["M-", "C-", "S-", "M1-", "M2-", "M3-", "M4-", "M5-"]

surround :: String -> String
surround s = "<" <> s <> ">"

-----------------------------------------------------------------------
-- Newtypes and Arbitrary instances

newtype AKeyMask = AKeyMask KeyMask
  deriving newtype (Show)

instance Arbitrary AKeyMask where
  arbitrary :: Gen AKeyMask
  arbitrary = fmap (coerce . sum . nub) . listOf . elements $
    [noModMask, shiftMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask]

newtype AKeySym = AKeySym KeySym
  deriving newtype (Show)

instance Arbitrary AKeySym where
  arbitrary :: Gen AKeySym
  arbitrary = elements . coerce . map snd $ regularKeys <> allSpecialKeys

newtype KeyString = KeyString String
  deriving newtype (Show)

instance Arbitrary KeyString where
  arbitrary :: Gen KeyString
  arbitrary = coerce . unwords <$> listOf keybinding
   where
    keybinding :: Gen String
    keybinding = do
      let keyStr = map fst $ regularKeys <> allSpecialKeys
      mks <- nub <$> listOf (elements ("" : mods))
      k   <- elements keyStr
      ks  <- listOf . elements $ keyStr
      pure $ concat mks <> k <> " " <> unwords ks
