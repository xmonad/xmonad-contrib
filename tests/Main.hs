module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified ExtensibleConf
import qualified ManageDocks
import qualified NoBorders
import qualified RotateSome
import qualified Selective
import qualified SwapWorkspaces
import qualified XPrompt
import qualified CycleRecentWS
import qualified OrgMode
import qualified GridSelect
import qualified EZConfig
import qualified WindowNavigation

main :: IO ()
main = hspec $ do
    context "ManageDocks" $ do
        prop "prop_r2c_c2r" ManageDocks.prop_r2c_c2r
        prop "prop_c2r_r2c" ManageDocks.prop_c2r_r2c
    context "Selective" $ do
        prop "prop_select_length"     Selective.prop_select_length
        prop "prop_update_idem"       Selective.prop_update_idem
        prop "prop_select_master"     Selective.prop_select_master
        prop "prop_select_focus"      Selective.prop_select_focus
        prop "prop_select_increasing" Selective.prop_select_increasing
        prop "prop_select_two_consec" Selective.prop_select_two_consec
        prop "prop_update_nm"         Selective.prop_update_nm
        prop "prop_update_start"      Selective.prop_update_start
        prop "prop_update_nr"         Selective.prop_update_nr
        prop "prop_update_focus_up"   Selective.prop_update_focus_up
        prop "prop_update_focus_down" Selective.prop_update_focus_down
    context "RotateSome" $ do
        prop "prop_rotate_some_length"  RotateSome.prop_rotate_some_length
        prop "prop_rotate_some_cycle"   RotateSome.prop_rotate_some_cycle
        prop "prop_rotate_some_anchors" RotateSome.prop_rotate_some_anchors
        prop "prop_rotate_some_rotate"  RotateSome.prop_rotate_some_rotate
        prop "prop_rotate_some_focus"   RotateSome.prop_rotate_some_focus
    context "SwapWorkspaces" $ do
        prop "prop_double_swap"       SwapWorkspaces.prop_double_swap
        prop "prop_invalid_swap"      SwapWorkspaces.prop_invalid_swap
        prop "prop_swap_only_two"     SwapWorkspaces.prop_swap_only_two
        prop "prop_swap_with_current" SwapWorkspaces.prop_swap_with_current
    context "XPrompt" $ do
        prop "prop_split"            XPrompt.prop_split
        prop "prop_spliInSubListsAt" XPrompt.prop_spliInSubListsAt
        prop "prop_skipGetLastWord"  XPrompt.prop_skipGetLastWord
    context "NoBorders"      NoBorders.spec
    context "ExtensibleConf" ExtensibleConf.spec
    context "CycleRecentWS"  CycleRecentWS.spec
    context "OrgMode"        OrgMode.spec
    context "GridSelect"     GridSelect.spec
    context "EZConfig"       EZConfig.spec
    context "WindowNavigation" WindowNavigation.spec
