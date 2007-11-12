{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
------------------------------------------------------------------------

module XMonad.Config.Droundy where

--
-- Useful imports
--
import XMonad hiding (keys)
import qualified XMonad (keys)
import XMonad.Config ( defaultConfig )

import XMonad.Layouts hiding ( (|||) )
import XMonad.Operations
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib

-- % Extension-provided imports

import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Square
import XMonad.Layout.LayoutScreens
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Shell

import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.RotView

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
                             ,height=22}


------------------------------------------------------------------------
-- Key bindings:

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- (The comment formatting character is used when generating the manpage)
--
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys x = M.fromList $
    -- launching and killing programs
    [ ((modMask x .|. shiftMask, xK_c     ), kill1) -- %! Close the focused window

    , ((modMask x,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask x .|. shiftMask, xK_space ), setLayout $ layoutHook x) -- %!  Reset the layouts on the current workspace to default

    -- move focus up or down the window stack
    , ((modMask x,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask x,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask x,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window

    , ((modMask x .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask x .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- floating layer support
    , ((modMask x,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask x .|. shiftMask, xK_Escape), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask x              , xK_Escape), broadcastMessage ReleaseResources >> restart (Just "xmonad") True) -- %! Restart xmonad

    , ((modMask x .|. shiftMask, xK_z     ),
       layoutScreens 1 (fixedLayout [Rectangle 0 0 1024 768]))
    , ((modMask x .|. shiftMask .|. controlMask, xK_z),
       layoutScreens 1 (fixedLayout [Rectangle 0 0 1440 900]))
    , ((modMask x .|. shiftMask, xK_Right), rotView True)
    , ((modMask x .|. shiftMask, xK_Left), rotView False)
    , ((modMask x, xK_Right), sendMessage $ Go R)
    , ((modMask x, xK_Left), sendMessage $ Go L)
    , ((modMask x, xK_Up), sendMessage $ Go U)
    , ((modMask x, xK_Down), sendMessage $ Go D)
    , ((modMask x .|. controlMask, xK_Right), sendMessage $ Swap R)
    , ((modMask x .|. controlMask, xK_Left), sendMessage $ Swap L)
    , ((modMask x .|. controlMask, xK_Up), sendMessage $ Swap U)
    , ((modMask x .|. controlMask, xK_Down), sendMessage $ Swap D)
    , ((modMask x .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modMask x .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
    , ((modMask x .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
    , ((modMask x .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)
 
    , ((0, xK_F2  ), spawn "gnome-terminal") -- %! Launch gnome-terminal
    , ((0, xK_F3  ), shellPrompt myXPConfig) -- %! Launch program
    , ((0, xK_F11   ), spawn "ksnapshot") -- %! Take snapshot
    , ((modMask x .|. shiftMask, xK_x     ), changeDir myXPConfig)
    , ((modMask x .|. shiftMask, xK_BackSpace), removeWorkspace)
    , ((modMask x .|. shiftMask, xK_v     ), selectWorkspace myXPConfig)
    , ((modMask x, xK_m     ), withWorkspace myXPConfig (windows . W.shift))
    , ((modMask x .|. shiftMask, xK_m     ), withWorkspace myXPConfig (windows . copy))
    , ((modMask x .|. shiftMask, xK_r), renameWorkspace myXPConfig)
    , ((modMask x, xK_l ), layoutPrompt myXPConfig)
    , ((modMask x .|. controlMask, xK_space), sendMessage ToggleLayout)
    ]
 
    ++
    zip (zip (repeat $ modMask x) [xK_F1..xK_F12]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modMask x .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace copy) [0..])

config = defaultConfig
         { borderWidth = 1 -- Width of the window border in pixels.
         , XMonad.workspaces = ["1:mutt","2:iceweasel"]
         , layoutHook = workspaceDir "~" $ windowNavigation $ toggleLayouts (noBorders Full) $
                        Named "tabbed" (noBorders mytab) |||
                        Named "xclock" (mytab <-/> combineTwo Square mytab mytab) |||
                        mytab <//> mytab
         , terminal = "xterm" -- The preferred terminal program.
         , normalBorderColor = "#dddddd" -- Border color for unfocused windows.
         , focusedBorderColor = "#00ff00" -- Border color for focused windows.
         , XMonad.modMask = mod1Mask
         , XMonad.keys = keys
         }
    where mytab = tabbed shrinkText defaultTConf

