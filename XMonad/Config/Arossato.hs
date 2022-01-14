{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Arossato
-- Description :  Andrea Rossato's xmonad configuration.
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
    {-# DEPRECATED "This module contains a personal configuration, to be removed from xmonad-contrib.  If you use this module, please copy the relevant parts to your configuration or obtain a copy of it on https://xmonad.org/configurations.html and include it as a local module." #-}
    ( -- * Usage
      -- $usage
      arossatoConfig
    ) where

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Layout.Accordion
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.Util.Themes

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
-- > main = xmonad =<< arossatoConfig
--
-- NOTE: that I'm using xmobar and, if you don't have xmobar in your
-- PATH, this configuration will produce an error and xmonad will not
-- start. If you don't want to install xmobar get rid of this line at
-- the beginning of 'arossatoConfig'.
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
-- >     ) where
--
-- to
--
-- > module Main where
--
-- 2. Add a line like:
--
-- > main = xmonad =<< arossatoConfig
--
-- 3. Start playing with the configuration options...;)

arossatoConfig = do
    xmobar <- spawnPipe "xmobar" -- REMOVE this line if you do not have xmobar installed!
    return $ def
         { workspaces         = ["home","var","dev","mail","web","doc"] ++
                                map show [7 .. 9 :: Int]
         , logHook            = myDynLog xmobar -- REMOVE this line if you do not have xmobar installed!
         , manageHook         = newManageHook
         , layoutHook         = avoidStruts $
                                decorated        |||
                                noBorders mytabs |||
                                otherLays
         , terminal           = "urxvt +sb"
         , normalBorderColor  = "white"
         , focusedBorderColor = "black"
         , keys               = newKeys
         , handleEventHook    = serverModeEventHook
         , focusFollowsMouse  = False
         }
    where
      -- layouts
      mytabs    =       tabbed shrinkText (theme smallClean)
      decorated = simpleFloat' shrinkText (theme smallClean)
      tiled     = Tall 1 (3/100) (1/2)
      otherLays = windowArrange   $
                  magnifier tiled |||
                  noBorders Full  |||
                  Mirror tiled    |||
                  Accordion

      -- manageHook
      myManageHook  = composeAll [ resource =? "win"          --> doF (W.shift "doc") -- xpdf
                                 , resource =? "firefox-bin"  --> doF (W.shift "web")
                                 ]
      newManageHook = myManageHook

      -- xmobar
      myDynLog    h = dynamicLogWithPP def
                      { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                      , ppTitle   = xmobarColor "green"  "" . shorten 40
                      , ppVisible = wrap "(" ")"
                      , ppOutput  = hPutStrLn h
                      }

      -- key bindings stuff
      defKeys    = keys def
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
          [ ((modMask x              , xK_F12   ), xmonadPrompt      def                 )
          , ((modMask x              , xK_F3    ), shellPrompt       def                 )
          , ((modMask x              , xK_F4    ), sshPrompt         def                 )
          , ((modMask x              , xK_F5    ), themePrompt       def                 )
          , ((modMask x              , xK_F6    ), windowPrompt def Goto  allWindows     )
          , ((modMask x              , xK_F7    ), windowPrompt def Bring allWindows     )
          , ((modMask x              , xK_comma ), prevWS                                )
          , ((modMask x              , xK_period), nextWS                                )
          , ((modMask x              , xK_Right ), windows W.focusDown                   )
          , ((modMask x              , xK_Left  ), windows W.focusUp                     )
          -- other stuff: launch some useful utilities
          , ((modMask x              , xK_F2    ), spawn "urxvt -fg white -bg black +sb" )
          , ((modMask x .|. shiftMask, xK_F4    ), spawn "~/bin/dict.sh"                 )
          , ((modMask x .|. shiftMask, xK_F5    ), spawn "~/bin/urlOpen.sh"              )
          , ((modMask x .|. shiftMask, xK_t     ), spawn "~/bin/teaTime.sh"              )
          , ((modMask x              , xK_c     ), kill                                  )
          , ((modMask x .|. shiftMask, xK_comma ), sendMessage (IncMasterN   1 )         )
          , ((modMask x .|. shiftMask, xK_period), sendMessage (IncMasterN (-1))         )
          -- commands fo the Magnifier layout
          , ((modMask x .|. controlMask              , xK_plus ), sendMessage MagnifyMore)
          , ((modMask x .|. controlMask              , xK_minus), sendMessage MagnifyLess)
          , ((modMask x .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
          , ((modMask x .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
          -- windowArranger
          , ((modMask x .|. controlMask              , xK_a    ), sendMessage  Arrange           )
          , ((modMask x .|. controlMask .|. shiftMask, xK_a    ), sendMessage  DeArrange         )
          , ((modMask x .|. controlMask              , xK_Left ), sendMessage (DecreaseLeft   10))
          , ((modMask x .|. controlMask              , xK_Up   ), sendMessage (DecreaseUp     10))
          , ((modMask x .|. controlMask              , xK_Right), sendMessage (IncreaseRight  10))
          , ((modMask x .|. controlMask              , xK_Down ), sendMessage (IncreaseDown   10))
          , ((modMask x .|. shiftMask                , xK_Left ), sendMessage (MoveLeft       10))
          , ((modMask x .|. shiftMask                , xK_Right), sendMessage (MoveRight      10))
          , ((modMask x .|. shiftMask                , xK_Down ), sendMessage (MoveDown       10))
          , ((modMask x .|. shiftMask                , xK_Up   ), sendMessage (MoveUp         10))
          -- gaps
          , ((modMask x                              , xK_b    ), sendMessage  ToggleStruts      )

          ] ++
          -- Use modMask .|. shiftMask .|. controlMask 1-9 instead
          [( (m .|. modMask x, k), windows $ f i)
           | (i, k) <- zip (workspaces x) [xK_1 .. xK_9]
          ,  (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]
          ]
