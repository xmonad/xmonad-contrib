{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Droundy
-- Description :  David Roundy's xmonad config.
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
------------------------------------------------------------------------
module XMonad.Config.Droundy {-# DEPRECATED "This module contains a personal configuration, to be removed from xmonad-contrib.  If you use this module, please copy the relevant parts to your configuration or obtain a copy of it on https://xmonad.org/configurations.html and include it as a local module." #-} ( config, mytab ) where

import XMonad hiding (keys, config)
import qualified XMonad (keys)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit ( exitSuccess )

import XMonad.Layout.Tabbed ( tabbed,
                              shrinkText, Shrinker, shrinkIt, CustomShrink(CustomShrink) )
import XMonad.Layout.Combo ( combineTwo )
import XMonad.Layout.Named ( named )
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Square ( Square(Square) )
import XMonad.Layout.WindowNavigation ( Navigate(Move,Swap,Go), Direction2D(U,D,R,L),
                                        windowNavigation )
import XMonad.Layout.BoringWindows ( boringWindows, markBoring, clearBoring,
                                     focusUp, focusDown )
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.WorkspaceDir ( changeDir, workspaceDir )
import XMonad.Layout.ToggleLayouts ( toggleLayouts, ToggleLayout(ToggleLayout) )
import XMonad.Layout.ShowWName ( showWName )
import XMonad.Layout.Magnifier ( maximizeVertical, MagnifyMsg(Toggle) )

import XMonad.Prompt ( font, height, XPConfig )
import XMonad.Prompt.Layout ( layoutPrompt )
import XMonad.Prompt.Shell ( shellPrompt )

import XMonad.Actions.CopyWindow ( kill1, copy )
import XMonad.Actions.DynamicWorkspaces ( withNthWorkspace, withWorkspace,
                                          selectWorkspace, renameWorkspace, removeWorkspace )
import XMonad.Actions.CycleWS ( moveTo, hiddenWS, emptyWS,
                                Direction1D( Prev, Next), WSType ((:&:), Not) )

import XMonad.Hooks.ManageDocks ( avoidStruts, docks )
import XMonad.Hooks.EwmhDesktops ( ewmh )

myXPConfig :: XPConfig
myXPConfig = def {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
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

    , ((modMask x .|. shiftMask, xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask x .|. controlMask .|. shiftMask, xK_L ), setLayout $ layoutHook x) -- %!  Reset the layouts on the current workspace to default

    -- move focus up or down the window stack
    , ((modMask x,               xK_Tab   ), focusDown) -- %! Move focus to the next window
    , ((modMask x,               xK_j     ), focusDown) -- %! Move focus to the next window
    , ((modMask x,               xK_k     ), focusUp  ) -- %! Move focus to the previous window

    , ((modMask x .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask x .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- floating layer support
    , ((modMask x,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask x .|. shiftMask, xK_Escape), io exitSuccess) -- %! Quit xmonad
    , ((modMask x              , xK_Escape), restart "xmonad" True) -- %! Restart xmonad

    , ((modMask x .|. shiftMask, xK_Right), moveTo Next $ hiddenWS :&: Not emptyWS)
    , ((modMask x .|. shiftMask, xK_Left), moveTo Prev $ hiddenWS :&: Not emptyWS)
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
    , ((modMask x .|. shiftMask, xK_b     ), markBoring)
    , ((controlMask .|. modMask x .|. shiftMask, xK_b     ), clearBoring)
    , ((modMask x .|. shiftMask, xK_x     ), changeDir myXPConfig)
    , ((modMask x .|. shiftMask, xK_BackSpace), removeWorkspace)
    , ((modMask x .|. shiftMask, xK_v     ), selectWorkspace myXPConfig)
    , ((modMask x, xK_m     ), withWorkspace myXPConfig (windows . W.shift))
    , ((modMask x .|. shiftMask, xK_m     ), withWorkspace myXPConfig (windows . copy))
    , ((modMask x .|. shiftMask, xK_r), renameWorkspace myXPConfig)
    , ((modMask x, xK_l ), layoutPrompt myXPConfig)
    , ((modMask x .|. controlMask, xK_space), sendMessage ToggleLayout)
    , ((modMask x, xK_space), sendMessage Toggle)

    ]

    ++
    zip (zip (repeat $ modMask x) [xK_F1..xK_F12]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modMask x .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace copy) [0..])

config = docks $ ewmh def
         { borderWidth = 1 -- Width of the window border in pixels.
         , XMonad.workspaces = ["mutt","iceweasel"]
         , layoutHook = showWName $ workspaceDir "~" $
                        boringWindows $ smartBorders $ windowNavigation $
                        maximizeVertical $ toggleLayouts Full $ avoidStruts $
                        named "tabbed" mytab |||
                        named "xclock" (mytab ****//* combineTwo Square mytab mytab) |||
                        named "three" (mytab **//* mytab *//* combineTwo Square mytab mytab) |||
                        named "widescreen" ((mytab *||* mytab)
                                                ****//* combineTwo Square mytab mytab) --   |||
                        --mosaic 0.25 0.5
         , terminal = "xterm" -- The preferred terminal program.
         , normalBorderColor = "#222222" -- Border color for unfocused windows.
         , focusedBorderColor = "#00ff00" -- Border color for focused windows.
         , XMonad.modMask = mod1Mask
         , XMonad.keys = keys
         }

mytab = tabbed CustomShrink def

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
dropFromTail "" _ = Nothing
dropFromTail t s | drop (length s - length t) s == t = Just $ take (length s - length t) s
                 | otherwise = Nothing

dropFromHead :: String -> String -> Maybe String
dropFromHead "" _ = Nothing
dropFromHead h s | take (length h) s == h = Just $ drop (length h) s
                 | otherwise = Nothing

{-
data FocusUrgencyHook = FocusUrgencyHook deriving (Read, Show)

instance UrgencyHook FocusUrgencyHook Window where
    urgencyHook _ w = modify copyAndFocus
        where copyAndFocus s
                  | Just w == W.peek (windowset s) = s
                  | has w $ W.stack $ W.workspace $ W.current $ windowset s =
                      s { windowset = until ((Just w ==) . W.peek)
                                      W.focusUp $ windowset s }
                  | otherwise =
                      let t = W.currentTag $ windowset s
                      in s { windowset = until ((Just w ==) . W.peek)
                             W.focusUp $ copyWindow w t $ windowset s }
              has _ Nothing         = False
              has x (Just (W.Stack t l rr)) = x `elem` (t : l ++ rr)

-}
