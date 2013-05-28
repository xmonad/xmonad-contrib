{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Sjanssen (sjanssenConfig) where

import XMonad hiding (Tall(..))
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt
import XMonad.Actions.SpawnOn
import XMonad.Util.SpawnOnce

import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane

import qualified Data.Map as M

sjanssenConfig =
    ewmh $ def
        { terminal = "exec urxvt"
        , workspaces = ["irc", "web"] ++ map show [3 .. 9 :: Int]
        , mouseBindings = \(XConfig {modMask = modm}) -> M.fromList $
                [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modm, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modm.|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w)) ]
        , keys = \c -> mykeys c `M.union` keys def c
        , logHook = dynamicLogString sjanssenPP >>= xmonadPropLog
        , layoutHook  = modifiers layouts
        , manageHook  = composeAll [className =? x --> doShift w
                                    | (x, w) <- [ ("Firefox", "web")
                                                , ("Ktorrent", "7")
                                                , ("Amarokapp", "7")]]
                        <+> manageHook def <+> manageDocks <+> manageSpawn
                        <+> (isFullscreen --> doFullFloat)
        , startupHook = mapM_ spawnOnce spawns
        }
 where
    tiled     = HintedTile 1 0.03 0.5 TopLeft
    layouts   = (tiled Tall ||| (tiled Wide ||| Full)) ||| tabbed shrinkText myTheme
    modifiers = avoidStruts . smartBorders

    spawns = [ "xmobar"
             , "xset -b", "xset s off", "xset dpms 0 600 1200"
             , "nitrogen --set-tiled wallpaper/wallpaper.jpg"
             , "trayer --transparent true --expand true --align right "
               ++ "--edge bottom --widthtype request" ]

    mykeys (XConfig {modMask = modm}) = M.fromList $
        [((modm,               xK_p     ), shellPromptHere myPromptConfig)
        ,((modm .|. shiftMask, xK_Return), spawnHere =<< asks (terminal . config))
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
