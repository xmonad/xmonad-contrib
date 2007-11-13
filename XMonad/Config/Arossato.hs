{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Arossato
-- Copyright   :  (c) Andrea Rossato 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  stable
-- Portability :  portable
--
-- This module specifies my xmonad defaults.
--
------------------------------------------------------------------------

module XMonad.Config.Arossato where

import XMonad
import XMonad.Layouts
import XMonad.Operations
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.Xlib

import XMonad.Layout.Accordion
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig 

-- ion3 clean style
myTabConfig :: TConf
myTabConfig = defaultTConf {
                activeColor         = "#8a999e" 
              , inactiveColor       = "#545d75"
              , activeBorderColor   = "white"
              , inactiveBorderColor = "grey"
              , activeTextColor     = "white"
              , inactiveTextColor   = "grey"
              , tabSize             = 15
              }

------------------------------------------------------------------------
-- |
-- Key bindings:
--
-- I want to remove some of the default key bindings, such as those to
-- exit XMonad
defaultKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
defaultKeys x = M.fromList $
    -- launching and killing programs
    [ ((modMask x .|. shiftMask, xK_Return), spawn "xterm"                  )
    , ((modMask x .|. shiftMask, xK_c     ), kill                           )

    , ((modMask x,               xK_space ), sendMessage NextLayout         )
    , ((modMask x .|. shiftMask, xK_space ), setLayout $ layoutHook x       )

    , ((modMask x,               xK_n     ), refresh                        )

    -- move focus up or down the window stack
    , ((modMask x,               xK_Tab   ), windows W.focusDown            )
    , ((modMask x,               xK_m     ), windows W.focusMaster          )

    -- modifying the window order
    , ((modMask x,               xK_Return), windows W.swapMaster           )
    , ((modMask x .|. shiftMask, xK_j     ), windows W.swapDown             )
    , ((modMask x .|. shiftMask, xK_k     ), windows W.swapUp               )

    -- resizing the master/slave ratio
    , ((modMask x,               xK_h     ), sendMessage Shrink             )
    , ((modMask x,               xK_l     ), sendMessage Expand             )

    -- floating layer support
    , ((modMask x,               xK_t     ), withFocused $ windows . W.sink )

    -- increase or decrease number of windows in the master area
    , ((modMask x .|. shiftMask, xK_comma ), sendMessage (IncMasterN   1 )  )
    , ((modMask x .|. shiftMask, xK_period), sendMessage (IncMasterN (-1))  )

    -- toggle the status bar gap
    , ((modMask x              , xK_b     ), modifyGap (\i n -> let s = (defaultGaps x ++ repeat (0,0,0,0)) !! i in if n == s then (0,0,0,0) else s))

    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask x, k), windows $ f i)
        | (i, k) <- zip (workspaces x) [xK_1 ..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask x, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++ mykeys x

-- These are my personal key bindings
mykeys :: XConfig Layout -> [((KeyMask, KeySym), (X ()))]
mykeys x = 
    [ ((modMask x                 , xK_F12   ), xmonadPrompt myXPConfig               )
    , ((modMask x                 , xK_F3    ), shellPrompt  myXPConfig               )
    , ((modMask x                 , xK_F4    ), sshPrompt    myXPConfig               )
    , ((modMask x                 , xK_F5    ), windowPromptGoto  myXPConfig          )
    , ((modMask x .|. shiftMask   , xK_F5    ), windowPromptBring myXPConfig          )
    -- mod . mod ,
    , ((modMask x                 , xK_comma ), prevWS                                )
    , ((modMask x                 , xK_period), nextWS                                )
    -- mod left mod right
    , ((modMask x                 , xK_Right ), windows W.focusDown                   )
    , ((modMask x                 , xK_Left  ), windows W.focusUp                     )
    -- other stuff: launch some useful utilities
    , ((modMask x                 , xK_F2    ), spawn "urxvt -fg white -bg black +sb" )
    , ((modMask x .|. shiftMask   , xK_F4    ), spawn "~/bin/dict.sh"                 )
    , ((modMask x .|. shiftMask   , xK_F5    ), spawn "~/bin/urlOpen.sh"              )
    , ((modMask x                 , xK_c     ), kill                                  )
    ]


arossatoConfig = defaultConfig
         { borderWidth        = 1
         , workspaces         = map show [1 .. 9 :: Int]
         , logHook            = dynamicLogWithPP myPP
         , layoutHook         = noBorders mytab |||
                                noBorders Full  |||
                                tiled           ||| 
                                Mirror tiled    ||| 
                                Accordion
         , terminal           = "xterm"
         , normalBorderColor  = "white"
         , focusedBorderColor = "black"
         , modMask            = mod1Mask
         , keys               = defaultKeys
         , defaultGaps        = [(15,0,0,0)]
         }
    where mytab = tabbed shrinkText myTabConfig
          tiled = Tall 1 0.03 0.5
          myPP  = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                            , ppTitle   = xmobarColor "#00ee00" "" . shorten 80
                            }
