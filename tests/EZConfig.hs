module EZConfig (spec) where

import Control.Arrow (first)
import Test.Hspec
import XMonad
import XMonad.Prelude
import XMonad.Util.EZConfig
import XMonad.Util.Parser

spec :: Spec
spec = do
  context "parseKey" $ do
    let prepare = unzip . map (first surround)
        testParseKey (ns, ks) = traverse (runParser parseKey) ns `shouldBe` Just ks
    it "parses all regular keys"    $ testParseKey          regularKeys
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

regularKeys :: ([String], [KeySym])
regularKeys = unzip . map (first (: ""))
            $ zip ['!'    .. '~'   ] [xK_exclam       .. xK_asciitilde]
           ++ zip ['\xa0' .. '\xff'] [xK_nobreakspace .. xK_ydiaeresis]

-- | QuickCheck can handle the 8! combinations just fine.
modifiers :: [String]
modifiers = map concat $
  permutations ["M-", "C-", "S-", "M1-", "M2-", "M3-", "M4-", "M5-"]

surround :: String -> String
surround s = "<" <> s <> ">"
