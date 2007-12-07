{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Sjanssen (sjanssenConfig) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Layouts hiding (Tall)
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile
import XMonad.Operations
import XMonad.Config (defaultConfig)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import System.IO (hPutStrLn)

sjanssenConfig = do
    xmobar <- spawnPipe "xmobar"
    return $ defaultConfig
        { defaultGaps = [(15,0,0,0)]
        , terminal = "urxvt"
        , workspaces = ["irc", "web"] ++ map show [3 .. 7 :: Int] ++ ["mail", "im"]
        , logHook = dynamicLogWithPP $ sjanssenPP { ppOutput = hPutStrLn xmobar }
        , modMask = mod4Mask
        , mouseBindings = \(XConfig {modMask = modm}) -> M.fromList $
                [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modm, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modm.|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w)) ]
        , keys = \c -> mykeys c `M.union` keys defaultConfig c
        , layoutHook = smartBorders (tiled Tall ||| tiled Wide ||| Full ||| tabbed shrinkText myTConf)
        }
 where
    tiled   = HintedTile 1 0.03 0.5

    mykeys (XConfig {modMask = modm, workspaces = ws}) = M.fromList $
        [((modm,               xK_p     ), shellPrompt myPromptConfig)
        ,((modm .|. shiftMask, xK_c     ), kill1)
        ,((modm .|. shiftMask .|. controlMask, xK_c     ), kill)
        ,((modm .|. shiftMask, xK_0     ), windows $ \w -> foldr copy w ws)
        ]

    myFont = "xft:Bitstream Vera Sans Mono:pixelsize=10"
    myTConf = defaultTConf { fontName = myFont }
    myPromptConfig = defaultXPConfig
                        { position = Top
                        , font = myFont
                        , promptBorderWidth = 0 }
