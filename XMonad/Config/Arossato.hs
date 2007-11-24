{-# OPTIONS_GHC -fglasgow-exts -fno-warn-missing-signatures #-}
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

module XMonad.Config.Arossato
    ( -- * Usage
      -- $usage
      arossatoConfig
    , arossatoTabbedConfig
    ) where

import qualified Data.Map as M
import Graphics.X11.Xlib

import XMonad
import XMonad.Layouts
import XMonad.ManageHook
import XMonad.Operations
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Accordion
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

-- $usage
-- The simplest way to use this configuration module is to use an
-- @~\/.xmonad\/xmonad.hs@ like this:
--
-- > module Main (main) where
-- >
-- > import XMonad
-- > import XMonad.Config.Arossato (arossatoConfig)
-- >
-- > main :: IO ()
-- > main = xmonad arossatoConfig
--
--
-- You can use this module also as a starting point for writing your
-- own configuration module from scratch. Save it as your
-- @~\/.xmonad\/xmonad.hs@ and:
--
-- 1. Change the module name from
--
-- > module XMonad.Config.Arossato
-- >     ( -- * Usage
-- >       -- $usage
-- >       arossatoConfig
-- >     , arossatoTabbedConfig
-- >     ) where
--
-- to
--
-- > module Main where
--
-- 2. Add a line like:
--
-- > main = xmonad arossatoConfig
--
-- 3. Start playing with the configuration options...;)

-- | My configuration for the Tabbed Layout. Basically this is the
-- Ion3 clean style.
arossatoTabbedConfig :: TConf
arossatoTabbedConfig =
    defaultTConf { activeColor         = "#8a999e"
                 , inactiveColor       = "#545d75"
                 , activeBorderColor   = "white"
                 , inactiveBorderColor = "grey"
                 , activeTextColor     = "white"
                 , inactiveTextColor   = "grey"
                 , tabSize             = 15
                 }

arossatoConfig = defaultConfig
         { workspaces         = ["home","var","dev","mail","web","doc"] ++
                                map show [7 .. 9 :: Int]
         , logHook            = dynamicLogXmobar
         , manageHook         = newManageHook
         , layoutHook         = noBorders mytab |||
                                magnifier tiled |||
                                noBorders Full  |||
                                tiled           |||
                                Mirror tiled    |||
                                Accordion
         , terminal           = "urxvt +sb"
         , normalBorderColor  = "white"
         , focusedBorderColor = "black"
         , keys               = newKeys
         , defaultGaps        = [(15,0,0,0)]
         }
    where
      -- layouts
      mytab = tabbed shrinkText arossatoTabbedConfig
      tiled = Tall 1 (3/100) (1/2)

      -- manageHook
      myManageHook  = composeAll [ resource =? "realplay.bin" --> doFloat
                                 , resource =? "win"          --> doF (W.shift "doc") -- xpdf
                                 , resource =? "firefox-bin"  --> doF (W.shift "web")
                                 ]
      newManageHook = myManageHook <+> manageHook defaultConfig

      -- key bindings stuff
      defKeys    = keys defaultConfig
      delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
      newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
      -- remove some of the default key bindings
      toRemove x =
          [ (modMask x              , xK_j)
          , (modMask x              , xK_k)
          , (modMask x              , xK_p)
          , (modMask x .|. shiftMask, xK_p)
          , (modMask x .|. shiftMask, xK_q)
          , (modMask x              , xK_q)
          ] ++
          -- I want modMask .|. shiftMask 1-9 to be free!
          [(shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]]
      -- These are my personal key bindings
      toAdd x   =
          [ ((modMask x              , xK_F12   ), xmonadPrompt      defaultXPConfig     )
          , ((modMask x              , xK_F3    ), shellPrompt       defaultXPConfig     )
          , ((modMask x              , xK_F4    ), sshPrompt         defaultXPConfig     )
          , ((modMask x              , xK_F5    ), windowPromptGoto  defaultXPConfig     )
          , ((modMask x              , xK_F6    ), windowPromptBring defaultXPConfig     )
          , ((modMask x              , xK_comma ), prevWS                                )
          , ((modMask x              , xK_period), nextWS                                )
          , ((modMask x              , xK_Right ), windows W.focusDown                   )
          , ((modMask x              , xK_Left  ), windows W.focusUp                     )
          -- other stuff: launch some useful utilities
          , ((modMask x              , xK_F2    ), spawn "urxvt -fg white -bg black +sb" )
          , ((modMask x .|. shiftMask, xK_F4    ), spawn "~/bin/dict.sh"                 )
          , ((modMask x .|. shiftMask, xK_F5    ), spawn "~/bin/urlOpen.sh"              )
          , ((modMask x              , xK_c     ), kill                                  )
          , ((modMask x .|. shiftMask, xK_comma ), sendMessage (IncMasterN   1 )         )
          , ((modMask x .|. shiftMask, xK_period), sendMessage (IncMasterN (-1))         )
          -- commands fo the Magnifier layout
          , ((modMask x .|. controlMask              , xK_plus ), sendMessage MagnifyMore)
          , ((modMask x .|. controlMask              , xK_minus), sendMessage MagnifyLess)
          , ((modMask x .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
          , ((modMask x .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
          ] ++
          -- Use modMask .|. shiftMask .|. controlMask 1-9 instead
          [( (m .|. modMask x, k), windows $ f i)
           | (i, k) <- zip (workspaces x) [xK_1 .. xK_9]
          ,  (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]
          ]
