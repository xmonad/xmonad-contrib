{-# OPTIONS_GHC -fno-warn-missing-signatures -fglasgow-exts -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
------------------------------------------------------------------------

module XMonad.Config.Droundy ( config, mytab ) where

--
-- Useful imports
--
import XMonad hiding (keys, config)
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
import XMonad.Layout.Mosaic
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

-- keybindings for Mosaic:
    , ((controlMask .|. modMask x .|. shiftMask, xK_h), withFocused (sendMessage . tallWindow))
    , ((controlMask .|. modMask x .|. shiftMask, xK_l), withFocused (sendMessage . wideWindow))
    , ((modMask x .|. shiftMask, xK_h     ), withFocused (sendMessage . shrinkWindow))
    , ((modMask x .|. shiftMask, xK_l     ), withFocused (sendMessage . expandWindow))
    , ((modMask x .|. shiftMask, xK_s     ), withFocused (sendMessage . squareWindow))
    , ((modMask x .|. shiftMask, xK_o     ), withFocused (sendMessage . myclearWindow))
    , ((controlMask .|. modMask x .|. shiftMask, xK_o     ), withFocused (sendMessage . flexibleWindow))

    ]
 
    ++
    zip (zip (repeat $ modMask x) [xK_F1..xK_F12]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modMask x .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace copy) [0..])

config = defaultConfig
         { borderWidth = 1 -- Width of the window border in pixels.
         , XMonad.workspaces = ["1:mutt","2:iceweasel"]
         , layoutHook = workspaceDir "~" $ windowNavigation $
                        toggleLayouts (noBorders Full) $ -- avoidStruts $
                        Named "tabbed" (noBorders mytab) |||
                        Named "xclock" (mytab ****//* combineTwo Square mytab mytab) |||
                        Named "widescreen" ((mytab *||* mytab)
                                                ****//* combineTwo Square mytab mytab) -- |||
                        --mosaic 0.25 0.5
         , terminal = "xterm" -- The preferred terminal program.
         , normalBorderColor = "#dddddd" -- Border color for unfocused windows.
         , focusedBorderColor = "#00ff00" -- Border color for focused windows.
         , XMonad.modMask = mod1Mask
         , XMonad.keys = keys
         }

mytab = tabbed CustomShrink defaultTConf

instance Shrinker CustomShrink where
    shrinkIt shr s | Just s' <- dropFromHead " " s = shrinkIt shr s' 
    shrinkIt shr s | Just s' <- dropFromTail " " s = shrinkIt shr s' 
    shrinkIt shr s | Just s' <- dropFromTail "- Iceweasel" s = shrinkIt shr s' 
    shrinkIt shr s | Just s' <- dropFromTail "- KPDF" s = shrinkIt shr s' 
    shrinkIt shr s | Just s' <- dropFromHead "file://" s = shrinkIt shr s' 
    shrinkIt shr s | Just s' <- dropFromHead "http://" s = shrinkIt shr s' 
    shrinkIt _ s | n > 9 = s : map cut [2..(halfn-3)] ++ shrinkIt shrinkText s
                 where n = length s
                       halfn = n `div` 2
                       rs = reverse s
                       cut x = take (halfn - x) s ++ "..." ++ reverse (take (halfn-x) rs)
    shrinkIt _ s = shrinkIt shrinkText s

dropFromTail :: String -> String -> Maybe String
dropFromTail t s | drop (length s - length t) s == t = Just $ take (length s - length t) s
                 | otherwise = Nothing

dropFromHead :: String -> String -> Maybe String
dropFromHead h s | take (length h) s == h = Just $ drop (length h) s
                 | otherwise = Nothing
