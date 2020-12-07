module ManageDocks where
import           XMonad                         ( Rectangle )
import           XMonad.Hooks.ManageDocks

prop_r2c_c2r :: RectC -> Bool
prop_r2c_c2r r = r2c (c2r r) == r

prop_c2r_r2c :: Rectangle -> Bool
prop_c2r_r2c r = c2r (r2c r) == r
