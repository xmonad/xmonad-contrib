{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Bluetile
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- This is the default configuration of Bluetile
-- (<http://projects.haskell.org/bluetile/>). If you
-- are migrating from Bluetile to xmonad or want to create
-- a similar setup, then this will give you pretty much
-- the same thing, except for Bluetile's helper applications
-- such as the dock.
--
-----------------------------------------------------------------------------

module XMonad.Config.Bluetile (
    -- * Usage
    -- $usage
    bluetileConfig
    ) where

import XMonad

import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration

import XMonad.Actions.BluetileCommands
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.WindowMenu

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WorkspaceByPos

import XMonad.Config.Gnome

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.Exit
import XMonad.Prelude(when)

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Bluetile
-- > import XMonad.Util.Replace
-- >
-- > main = replace >> xmonad bluetileConfig
--
-- The invocation of 'replace' will replace a currently running
-- window manager. This is the default behaviour of Bluetile as well.
-- See "XMonad.Util.Replace" for more information.

bluetileWorkspaces :: [String]
bluetileWorkspaces = ["1","2","3","4","5","6","7","8","9","0"]

bluetileKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
bluetileKeys conf@(XConfig {XMonad.modMask = modMask'}) = M.fromList $
    -- launching and killing programs
    [ ((modMask'              , xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask',               xK_p     ), gnomeRun)    --  %! Launch Gnome "Run application" dialog
    , ((modMask' .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask',               xK_F5 ), refresh) -- %! Resize viewed windows to the correct size
    , ((modMask' .|. shiftMask, xK_F5 ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask',               xK_o ), windowMenu)

    -- move focus up or down the window stack
    , ((modMask',               xK_Tab   ), focusDown) -- %! Move focus to the next window
    , ((modMask' .|. shiftMask, xK_Tab   ), focusUp) -- %! Move focus to the previous window
    , ((modMask',               xK_j     ), focusDown) -- %! Move focus to the next window
    , ((modMask',               xK_k     ), focusUp) -- %! Move focus to the previous window
    , ((modMask',               xK_space ), focusMaster) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask' .|. shiftMask, xK_space ), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask' .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask' .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask',               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask',               xK_l     ), sendMessage Expand) -- %! Expand the master area
    , ((modMask',               xK_u     ), sendMessage ShrinkSlave) -- %! Shrink a slave area
    , ((modMask',               xK_i     ), sendMessage ExpandSlave) -- %! Expand a slave area

    -- floating layer support
    , ((modMask',               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling
    , ((modMask' .|. shiftMask, xK_t     ), withFocused $ float ) -- %! Float window

    -- increase or decrease number of windows in the master area
    , ((modMask'              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask'              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask' .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit
    , ((modMask'              , xK_q     ), restart "xmonad" True) -- %! Restart

    -- Metacity-like workspace switching
    , ((mod1Mask .|. controlMask, xK_Left), prevWS)
    , ((mod1Mask .|. controlMask, xK_Right), nextWS)
    , ((mod1Mask .|. controlMask .|. shiftMask,   xK_Left), shiftToPrev >> prevWS)
    , ((mod1Mask .|. controlMask .|. shiftMask,   xK_Right), shiftToNext >> nextWS)

    -- more Metacity keys
    , ((mod1Mask             , xK_F2), gnomeRun)
    , ((mod1Mask             , xK_F4), kill)

    -- Switching to layouts
    , ((modMask'              , xK_a), sendMessage $ JumpToLayout "Floating")
    , ((modMask'              , xK_s), sendMessage $ JumpToLayout "Tiled1")
    , ((modMask'              , xK_d), sendMessage $ JumpToLayout "Tiled2")
    , ((modMask'              , xK_f), sendMessage $ JumpToLayout "Fullscreen")

    -- Maximizing
    , ((modMask'              , xK_z), withFocused (sendMessage . maximizeRestore))

    -- Minimizing
    , ((modMask',               xK_m     ), withFocused minimizeWindow)
    , ((modMask' .|. shiftMask, xK_m     ), withLastMinimized maximizeWindow)
    ]
    ++
    -- mod-[1..9] ++ [0] %! Switch to workspace N
    -- mod-shift-[1..9] ++ [0] %! Move client to workspace N
    [((m .|. modMask', k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask', key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

bluetileMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
bluetileMouseBindings (XConfig {XMonad.modMask = modMask'}) = M.fromList $
    -- mod-button1 %! Move a floated window by dragging
    [ ((modMask', button1), (\w -> isFloating w >>= \isF -> when (isF) $
                                focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2 %! Switch to next and first layout
    , ((modMask', button2), (\_ -> sendMessage NextLayout))
    , ((modMask' .|. shiftMask, button2), (\_ -> sendMessage $ JumpToLayout "Floating"))
    -- mod-button3 %! Resize a floated window by dragging
    , ((modMask', button3), (\w -> isFloating w >>= \isF -> when (isF) $
                                focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

isFloating :: Window -> X (Bool)
isFloating w = do
    ws <- gets windowset
    return $ M.member w (W.floating ws)

bluetileManageHook :: ManageHook
bluetileManageHook = composeAll
               [ workspaceByPos, positionStoreManageHook (Just defaultThemeWithButtons)
                , className =? "MPlayer" --> doFloat
                , isFullscreen --> doFullFloat]

bluetileLayoutHook = avoidStruts $ minimize $ boringWindows $ (
                        named "Floating" floating |||
                        named "Tiled1" tiled1 |||
                        named "Tiled2" tiled2 |||
                        named "Fullscreen" full
                        )
        where
            floating = floatingDeco $ maximize $ borderResize $ positionStoreFloat
            tiled1 = tilingDeco $ maximize $ mouseResizableTileMirrored
            tiled2 = tilingDeco $ maximize $ mouseResizableTile
            full = tilingDeco $ maximize $ smartBorders Full

            tilingDeco l = windowSwitcherDecorationWithButtons shrinkText defaultThemeWithButtons (draggingVisualizer l)
            floatingDeco l = buttonDeco shrinkText defaultThemeWithButtons l

bluetileConfig =
    docks $
    ewmh' def{ fullscreen = True } $
    def
        { modMask = mod4Mask,   -- logo key
          manageHook = bluetileManageHook,
          layoutHook = bluetileLayoutHook,
          logHook = currentWorkspaceOnTop,
          handleEventHook = minimizeEventHook
                                `mappend` serverModeEventHook' bluetileCommands
                                `mappend` positionStoreEventHook,
          workspaces = bluetileWorkspaces,
          keys = bluetileKeys,
          mouseBindings = bluetileMouseBindings,
          focusFollowsMouse = False,
          focusedBorderColor = "#000000",
          terminal = "gnome-terminal"
        }
