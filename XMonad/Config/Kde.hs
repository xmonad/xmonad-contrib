{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Kde
-- Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>, (c) Bogdan Sinitsyn <bogdan.sinitsyn@gmail.com>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    :  unstable
-- Portability  :  unportable
--
-- This module provides a config suitable for use with the KDE desktop
-- environment.

module XMonad.Config.Kde (
    -- * Usage
    -- $usage
    kde5Config,
    kde4Config
    ) where

import Control.Monad                  (forM_, unless, when)
import Data.List                      (union)
import qualified Data.Map             as M
import Data.Maybe                     (fromMaybe)
import Data.Monoid                    (All(..))

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops      (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet      as W
import XMonad.Util.WindowProperties   (getProp32)
import XMonad.Util.XUtils             (fi)

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Kde
-- >
-- > main = xmonad $ kde5Config defaultConfig
--
-- For KDE 4, replace last line with
-- > main = xmonad kde4Config
--
-- For examples of how to further customize @kde4Config@ see "XMonad.Config.Desktop".


kde4Config = desktopConfig
    { terminal = "konsole"
    , keys     = kde4Keys <+> keys desktopConfig }

kde4Keys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "krunner")
    , ((modm .|. shiftMask, xK_q), spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1")
    ]

kde5Config :: LayoutClass λ Window
           => XConfig λ
           -> XConfig (ModifiedLayout AvoidStruts (ModifiedLayout SmartBorder λ))
kde5Config conf = ewmh $ conf
    { layoutHook = desktopLayoutModifiers .
                     smartBorders .
                       layoutHook $ conf
    , handleEventHook = composeAll [ docksEventHook
                                   , setBordersIf kdeOverride 0
                                   , setBordersIf isPlasmaOSD (borderWidth conf)
                                   , removeFromTaskbarIf (isPlasmaOSD <||> isNotification <||> kdeOverride)
                                   , handleEventHook conf ]
    , manageHook = composeAll [ isDesktop <||> isDock --> doIgnore
                              , isPlasmaOSD --> doSideFloat SC
                              , kdeOverride --> doFloat
                              , isNotification --> doIgnore
                              , manageHook conf
                              , (not <$> (kdeOverride <||> isPlasmaOSD)) --> manageDocks ]
    , startupHook = startupHook desktopConfig >> startupHook conf }


setBordersIf :: Query Bool -> Dimension -> Event -> X All
setBordersIf query width event = do
  whenX (runQuery query window) $
    io $ setWindowBorderWidth dpy window width
  return (All True)
  where
    dpy = ev_event_display event
    window = ev_window event

setBordersIf _ _ _ = return (All True)

isNotification = isInType     "_NET_WM_WINDOW_TYPE_NOTIFICATION"
isPlasmaOSD    = isInType "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY"
kdeOverride    = isInType "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
isDesktop      = isInType     "_NET_WM_WINDOW_TYPE_DESKTOP"
isDock         = isInType     "_NET_WM_WINDOW_TYPE_DOCK"

isInType :: String -> Query Bool
isInType = isInProperty "_NET_WM_WINDOW_TYPE"

removeFromTaskbarIf :: Query Bool -> Event -> X All
removeFromTaskbarIf query event = do
  whenX (runQuery query window) $ do
    wmstate <- getAtom "_NET_WM_STATE"
    wstate <- fromMaybe [] <$> getProp32 wmstate window
    skipTaskbar <- getAtom "_NET_WM_STATE_SKIP_TASKBAR"
    skipPager <- getAtom "_NET_WM_STATE_SKIP_PAGER"
    above <- getAtom "_NET_WM_STATE_ABOVE"
    let newwstate = union wstate (map fi [skipTaskbar, skipPager, above])
    when (newwstate /= wstate) $ do
      io $ changeProperty32 dpy window wmstate 4 propModeReplace newwstate
  return (All True)
  where
    dpy = ev_event_display event
    window = ev_window event

removeFromTaskbarIf _ _ = return (All True)
