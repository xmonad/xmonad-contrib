module XMonad.Config.Sjanssen (sjanssenConfig) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layouts
import XMonad.Layout.Tabbed
import XMonad.Operations
import XMonad.Config (defaultConfig)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.Bits
import qualified Data.Map as M
import Graphics.X11

sjanssenConfig :: XConfig
sjanssenConfig = defaultConfig
        { defaultGaps = [(15,0,0,0)]
        , terminal = "urxvt"
        , workspaces = ["irc", "web"] ++ map show [3 .. 7 :: Int] ++ ["mail", "im"]
        , logHook = dynamicLogWithPP sjanssenPP
        , modMask = mod4Mask
        , mouseBindings = \(XConfig {modMask = modm}) -> M.fromList $
                [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modm, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modm.|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w)) ]
        , keys = \c -> mykeys c `M.union` keys defaultConfig c
        , layoutHook = Layout (smartBorders (tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText defaultTConf))
        }
 where
    tiled   = Tall 1 0.03 0.5

    mykeys (XConfig {modMask = modm}) = M.fromList $
        [((modm,               xK_p     ), shellPrompt myPromptConfig)]

    myPromptConfig = defaultXPConfig
                        { position = Top
                        , promptBorderWidth = 0 }
