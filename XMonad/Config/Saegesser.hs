{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------------
-- |
-- A mostly striped down configuration that demonstrates spawnOnOnce
--
---------------------------------------------------------------------
module XMonad.Config.Saegesser {-# DEPRECATED "This module contains a personal configuration, to be removed from xmonad-contrib.  If you use this module, please copy the relevant parts to your configuration or obtain a copy of it on https://xmonad.org/configurations.html and include it as a local module." #-} where

import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Mosaic

import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce

import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn

import qualified XMonad.StackSet as W

main = do
  myStatusBarPipe <- spawnPipe "xmobar"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ def
    { terminal          = "xterm"
    , workspaces        = myWorkspaces
    , layoutHook        = myLayoutHook
    , manageHook        = myManageHook <+> manageSpawn
    , startupHook       = myStartupHook
    , logHook           = myLogHook myStatusBarPipe
    , focusFollowsMouse = False
    }

myManageHook = composeOne
  [ isDialog                     -?> doFloat
  , className =? "trayer"        -?> doIgnore
  , className =? "Skype"         -?> doShift "chat"
  , appName   =? "libreoffice"   -?> doShift "office"
  , return True                  -?> doF W.swapDown
  ]

myWorkspaces = [ "web", "emacs", "chat", "vm", "office", "media", "xterms", "8", "9", "0"]

myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnOnce "emacs" "emacs"
  spawnNOnOnce 4 "xterms" "xterm"

myLayoutHook = smartBorders $ avoidStruts standardLayouts
  where standardLayouts = tiled ||| mosaic 2 [3,2]  ||| Mirror tiled ||| Full
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 0.03
        ratio = 0.6

myLogHook p =  do
  copies <- wsContainingCopies
  let check ws | ws == "NSP" = ""                               -- Hide the scratchpad workspace
               | ws `elem` copies = xmobarColor "red" "black" ws  -- Workspaces with copied windows are red on black
               | otherwise = ws
  dynamicLogWithPP $ xmobarPP { ppHidden = check
                              , ppOutput = hPutStrLn p
                              , ppUrgent = xmobarColor "white" "red"
                              , ppTitle  = xmobarColor "green" "" . shorten 180
                              }
  fadeInactiveLogHook 0.6
