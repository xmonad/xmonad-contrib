{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Gnome
-- Description  : Config for integrating xmonad with GNOME.
-- Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    :  unstable
-- Portability  :  unportable
--
-- This module provides a config suitable for use with the GNOME desktop
-- environment.

module XMonad.Config.Gnome (
    -- * Usage
    -- $usage
    gnomeConfig,
    gnomeRun,
    gnomeRegister,
    desktopLayoutModifiers
    ) where

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)

import qualified Data.Map as M

import System.Environment (getEnvironment)

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Gnome
-- >
-- > main = xmonad gnomeConfig
--
-- For examples of how to further customize @gnomeConfig@ see "XMonad.Config.Desktop".

gnomeConfig = desktopConfig
    { terminal = "gnome-terminal"
    , keys     = gnomeKeys <+> keys desktopConfig
    , startupHook = gnomeRegister >> startupHook desktopConfig }

gnomeKeys XConfig{modMask = modm} = M.fromList
    [ ((modm, xK_p), gnomeRun)
    , ((modm .|. shiftMask, xK_q), spawn "gnome-session-quit --logout") ]

-- | Launch the "Run Application" dialog.  gnome-panel must be running for this
-- to work.
gnomeRun :: X ()
gnomeRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    gnome_panel <- getAtom "_GNOME_PANEL_ACTION"
    panel_run   <- getAtom "_GNOME_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw gnome_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

-- | Register xmonad with gnome. 'dbus-send' must be in the $PATH with which
-- xmonad is started.
--
-- This action reduces a delay on startup only only if you have configured
-- gnome-session>=2.26: to start xmonad with a command as such:
--
-- > gconftool-2 -s /desktop/gnome/session/required_components/windowmanager xmonad --type string
gnomeRegister :: MonadIO m => m ()
gnomeRegister = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" <$> getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.gnome.SessionManager"
            ,"/org/gnome/SessionManager"
            ,"org.gnome.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]
