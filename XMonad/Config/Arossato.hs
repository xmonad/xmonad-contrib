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

import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.Xlib

import XMonad
import XMonad.Layouts
import XMonad.Operations
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

-- The Ion3 clean style
myTabConfig :: TConf
myTabConfig = 
    defaultTConf { activeColor         = "#8a999e" 
                 , inactiveColor       = "#545d75"
                 , activeBorderColor   = "white"
                 , inactiveBorderColor = "grey"
                 , activeTextColor     = "white"
                 , inactiveTextColor   = "grey"
                 , tabSize             = 15
                 }

arossatoConfig = defaultConfig
         { workspaces         = ["1", "2"] ++
                                ["dev","mail","web"] ++
                                map show [6 .. 9 :: Int]
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
         , keys               = newKeys
         , defaultGaps        = [(15,0,0,0)]
         }
    where 
      -- layouts
      mytab = tabbed shrinkText myTabConfig
      tiled = Tall 1 0.03 0.5
      
      -- the logHook pretty-printer
      myPP  = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppTitle   = xmobarColor "green"  "" . shorten 80
                        }

      -- key bindings stuff
      defKeys    = keys defaultConfig
      delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
      newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
      -- remove some of the default key bindings
      toRemove x = 
          [ (modMask x              , xK_j     )
          , (modMask x              , xK_k     )
          , (modMask x              , xK_p     )
          , (modMask x .|. shiftMask, xK_p     )
          , (modMask x .|. shiftMask, xK_q     )
          , (modMask x              , xK_q     )
          , (modMask x              , xK_comma )
          , (modMask x              , xK_period)
          ] ++
          -- I want modMask .|. shiftMusk 1-9 to be free!
          [(shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]]

      -- These are my personal key bindings
      toAdd x   = 
          [ ((modMask x              , xK_F12   ), xmonadPrompt      defaultXPConfig    )
          , ((modMask x              , xK_F3    ), shellPrompt       defaultXPConfig    )
          , ((modMask x              , xK_F4    ), sshPrompt         defaultXPConfig    )
          , ((modMask x              , xK_F5    ), windowPromptGoto  defaultXPConfig    )
          , ((modMask x .|. shiftMask, xK_F5    ), windowPromptBring defaultXPConfig    )
          , ((modMask x              , xK_comma ), prevWS                               )
          , ((modMask x              , xK_period), nextWS                               )
          , ((modMask x              , xK_Right ), windows W.focusDown                  )
          , ((modMask x              , xK_Left  ), windows W.focusUp                    )
          -- other stuff: launch some useful utilities
          , ((modMask x              , xK_F2    ), spawn "urxvt -fg white -bg black +sb")
          , ((modMask x .|. shiftMask, xK_F4    ), spawn "~/bin/dict.sh"                )
          , ((modMask x .|. shiftMask, xK_F5    ), spawn "~/bin/urlOpen.sh"             )
          , ((modMask x              , xK_c     ), kill                                 )
          , ((modMask x .|. shiftMask, xK_comma ), sendMessage (IncMasterN   1 )        )
          , ((modMask x .|. shiftMask, xK_period), sendMessage (IncMasterN (-1))        )
          ] ++
          -- Use modMask .|. shiftMask .|. controlMask 1-9 instead
          [( (m .|. modMask x, k), windows $ f i)
           | (i, k) <- zip (workspaces x) [xK_1 .. xK_9]
          ,  (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]
          ]

