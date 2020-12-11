module Main where
import           Test.QuickCheck
import           System.Environment
import           Text.Printf
import           Control.Monad
import qualified ManageDocks
import qualified RotateSome
import qualified Selective
import qualified SwapWorkspaces

import qualified XPrompt

main :: IO ()
main = do
  arg <- fmap (drop 1) getArgs
  let n    = if null arg then 100 else read $ head arg
      args = stdArgs { maxSuccess = n, maxSize = 100, maxDiscardRatio = 100 }
      qc t = do
        c <- quickCheckWithResult args t
        case c of
          Success{} -> return True
          _         -> return False
      perform (s, t) = printf "%-35s: " s >> qc t
  nFailed <- length . filter not <$> mapM perform tests
  unless (nFailed == 0) (error (show nFailed ++ " test(s) failed"))


tests :: [(String, Property)]
tests =
  [ ("ManageDocks.prop_r2c_c2r"    , property ManageDocks.prop_r2c_c2r)
  , ("ManageDocks.prop_c2r_r2c"    , property ManageDocks.prop_c2r_r2c)
  , ("Selective.prop_select_length", property Selective.prop_select_length)
  , ("Selective.prop_update_idem"  , property Selective.prop_update_idem)
  , ("Selective.prop_select_master", property Selective.prop_select_master)
  , ("Selective.prop_select_focus" , property Selective.prop_select_focus)
  , ( "Selective.prop_select_increasing"
    , property Selective.prop_select_increasing
    )
  , ( "Selective.prop_select_two_consec"
    , property Selective.prop_select_two_consec
    )
  , ("Selective.prop_update_nm"      , property Selective.prop_update_nm)
  , ("Selective.prop_update_start"   , property Selective.prop_update_start)
  , ("Selective.prop_update_nr"      , property Selective.prop_update_nr)
  , ("Selective.prop_update_focus_up", property Selective.prop_update_focus_up)
  , ( "Selective.prop_update_focus_down"
    , property Selective.prop_update_focus_down
    )
  , ( "RotateSome.prop_rotate_some_length"
    , property RotateSome.prop_rotate_some_length
    )
  , ( "RotateSome.prop_rotate_some_cycle"
    , property RotateSome.prop_rotate_some_cycle
    )
  , ( "RotateSome.prop_rotate_some_anchors"
    , property RotateSome.prop_rotate_some_anchors
    )
  , ( "RotateSome.prop_rotate_some_rotate"
    , property RotateSome.prop_rotate_some_rotate
    )
  , ( "RotateSome.prop_rotate_some_focus"
    , property RotateSome.prop_rotate_some_focus
    )
  , ( "SwapWorkspaces.prop_double_swap"
    , property SwapWorkspaces.prop_double_swap
    )
  , ( "SwapWorkspaces.prop_invalid_swap"
    , property SwapWorkspaces.prop_invalid_swap
    )
  , ( "SwapWorkspaces.prop_swap_only_two"
    , property SwapWorkspaces.prop_swap_only_two
    )
  , ( "SwapWorkspaces.prop_swap_with_current"
    , property SwapWorkspaces.prop_swap_with_current
    )
  , ("XPrompt.prop_split"           , property XPrompt.prop_split)
  , ("XPrompt.prop_spliInSubListsAt", property XPrompt.prop_spliInSubListsAt)
  , ("XPrompt.prop_skipGetLastWord" , property XPrompt.prop_skipGetLastWord)
  ]
