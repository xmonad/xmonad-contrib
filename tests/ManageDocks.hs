module ManageDocks where
import XMonad
import XMonad.Hooks.ManageDocks
import Test.QuickCheck
import Foreign.C.Types
import Properties

instance Arbitrary CLong where
    arbitrary = fromIntegral `fmap` (arbitrary :: Gen Int)
instance Arbitrary RectC where
    arbitrary = do
        (x,y) <- arbitrary
        NonNegative w <- arbitrary
        NonNegative h <- arbitrary
        return $ RectC (x,y,x+w,y+h)

prop_r2c_c2r :: RectC -> Bool
prop_r2c_c2r r = r2c (c2r r) == r

prop_c2r_r2c :: Rectangle -> Bool
prop_c2r_r2c r = c2r (r2c r) == r
