{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Sjanssen (sjanssenConfig) where

import XMonad hiding (Tall(..))
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile
import XMonad.Config (defaultConfig)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt
import XMonad.Actions.SpawnOn

import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane

import qualified Data.Map as M

sjanssenConfig = do
    sp <- mkSpawner :: IO Spawner
    return . ewmh $ defaultConfig
        { terminal = "exec urxvt"
        , workspaces = ["irc", "web"] ++ map show [3 .. 9 :: Int]
        , mouseBindings = \(XConfig {modMask = modm}) -> M.fromList $
                [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modm, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modm.|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w)) ]
        , keys = \c -> mykeys sp c `M.union` keys defaultConfig c
        , logHook = dynamicLogString sjanssenPP >>= xmonadPropLog
        , layoutHook  = modifiers layouts
        , manageHook  = composeAll [className =? x --> doShift w
                                    | (x, w) <- [ ("Firefox", "web")
                                                , ("Ktorrent", "7")
                                                , ("Amarokapp", "7")]]
                        <+> manageHook defaultConfig <+> manageDocks <+> manageSpawn sp
                        <+> (isFullscreen --> doFullFloat)
        }
 where
    tiled     = HintedTile 1 0.03 0.5 TopLeft
    layouts   = (tiled Tall ||| (tiled Wide ||| Full)) ||| tabbed shrinkText myTheme
    modifiers = avoidStruts . smartBorders

    mykeys sp (XConfig {modMask = modm}) = M.fromList $
        [((modm,               xK_p     ), shellPromptHere sp myPromptConfig)
        ,((modm .|. shiftMask, xK_Return), spawnHere sp =<< asks (terminal . config))
        ,((modm .|. shiftMask, xK_c     ), kill1)
        ,((modm .|. shiftMask .|. controlMask, xK_c     ), kill)
        ,((modm .|. shiftMask, xK_0     ), windows $ copyToAll)
        ,((modm,               xK_z     ), layoutScreens 2 $ TwoPane 0.5 0.5)
        ,((modm .|. shiftMask, xK_z     ), rescreen)
        , ((modm             , xK_b     ), sendMessage ToggleStruts)
        ]

    myFont = "xft:Bitstream Vera Sans Mono:pixelsize=10"
    myTheme = defaultTheme { fontName = myFont }
    myPromptConfig = defaultXPConfig
                        { position = Top
                        , font = myFont
                        , showCompletionOnTab = True
                        , historyFilter = deleteConsecutive
                        , promptBorderWidth = 0 }
