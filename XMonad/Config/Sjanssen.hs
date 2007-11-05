module XMonad.Config.Sjanssen (sjanssenConfig) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layouts
import XMonad.Layout.Tabbed
import XMonad.Operations
import XMonad.DefaultConfig (defaultConfig)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.Ratio
import Data.Bits
import qualified Data.Map as M
import Graphics.X11

sjanssenConfig = defaultConfig
        { defaultGaps = [(15,0,0,0)]
        , terminal = "urxvt"
        , workspaces = ["irc", "web"] ++ map show [3..7] ++ ["mail", "im"]
        , logHook = dynamicLogWithPP sjanssenPP
        , modMask = mod4Mask
        , mouseBindings = \(XConfig {modMask = modMask}) -> M.fromList $
                [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modMask .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w)) ]
        , keys = \c -> mykeys c `M.union` keys defaultConfig c
        , layoutHook = Layout (smartBorders (tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText defaultTConf))
        }
 where
    mykeys (XConfig {modMask = modMask}) = M.fromList $
        [((modMask,               xK_p     ), shellPrompt myPromptConfig)]
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1%2

    -- Percent of screen to increment by when resizing panes
    delta   = 3%100

myPromptConfig = defaultXPConfig
                    { position = Top
                    , promptBorderWidth = 0
                    }
