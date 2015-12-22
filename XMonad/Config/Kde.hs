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

import qualified Data.Map             as M
import Data.Monoid                    (All(..))

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops      (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
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
-- Don't use @kde5Config@ from this module with @transience@ (or @transience'@) from
-- "XMonad.Hooks.ManageHelpers"


kde4Config = desktopConfig
    { terminal = "konsole"
    , keys     = kde4Keys <+> keys desktopConfig }

kde4Keys XConfig {modMask = modm} = M.fromList
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
                                   , skipIf kdeOverride
                                   , handleEventHook conf ]
    , manageHook = composeAll [ isDesktop <||> isDock --> doIgnore
                              , isPlasmaOSD <||> isNotification --> doIgnore
                              , kdeOverride --> doFloat
                              , (not <$> isFloating) --> manageDocks
                              , manageHook conf ]
    , startupHook = startupHook desktopConfig >> startupHook conf }


isFloating :: Query Bool
isFloating = ask >>= \w -> do
  ws <- liftX $ gets windowset
  return $ w `elem` M.keys (W.floating ws)

setBordersIf :: Query Bool -> Dimension -> Event -> X All
setBordersIf query width event = do
  whenX (runQuery query window) $
    io $ setWindowBorderWidth dpy window width
  return (All True)
  where
    dpy = ev_event_display event
    window = ev_window event

isNotification = isInType     "_NET_WM_WINDOW_TYPE_NOTIFICATION"
isPlasmaOSD    = isInType "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY"
kdeOverride    = isInType "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
isDesktop      = isInType     "_NET_WM_WINDOW_TYPE_DESKTOP"
isDock         = isInType     "_NET_WM_WINDOW_TYPE_DOCK"

isInType :: String -> Query Bool
isInType = isInProperty "_NET_WM_WINDOW_TYPE"

skipIf :: Query Bool -> Event -> X All
skipIf query event = do
  whenX (runQuery query window) $ do
    wmstate <- getAtom "_NET_WM_STATE"
    atom <- getAtom "ATOM"
    skip <- mapM getAtom [ "_NET_WM_STATE_SKIP_TASKBAR"
                         , "_NET_WM_STATE_SKIP_PAGER"
                         , "_NET_WM_STATE_ABOVE"]
    io $ changeProperty32 dpy window wmstate atom propModeAppend (map fi skip)
  return (All True)
  where
    dpy = ev_event_display event
    window = ev_window event
