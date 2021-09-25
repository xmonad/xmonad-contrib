{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Mate
-- Description  : Config for integrating xmonad with MATE.
-- Copyright    : (c) Brandon S Allbery KF8NH, 2014
-- License      : BSD
--
-- Maintainer   : allbery.b@gmail.com
-- Stability    :  unstable
-- Portability  :  unportable
--
-- This module provides a config suitable for use with the MATE desktop
-- environment.
--
-----------------------------------------------------------------------------

module XMonad.Config.Mate (
    -- * Usage
    -- $usage
    mateConfig,
    mateRun,
    matePanel,
    mateRegister,
    mateLogout,
    mateShutdown,
    desktopLayoutModifiers
    ) where

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Ungrab
import XMonad.Prelude (toUpper)

import qualified Data.Map as M

import System.Environment (getEnvironment)

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Mate
-- >
-- > main = xmonad mateConfig
--
-- For examples of how to further customize @mateConfig@ see "XMonad.Config.Desktop".

mateConfig = desktopConfig
    { terminal = "mate-terminal"
    , keys     = mateKeys <+> keys desktopConfig
    , startupHook = mateRegister >> startupHook desktopConfig }

mateKeys XConfig{modMask = modm} = M.fromList
    [ ((modm, xK_p), mateRun)
    , ((modm, xK_d), unGrab >> matePanel "MAIN_MENU")
    , ((modm .|. shiftMask, xK_q), mateLogout) ]

-- | Launch the "Run Application" dialog.  mate-panel must be running for this
-- to work.  partial application for existing keybinding compatibility.
mateRun :: X ()
mateRun = matePanel "RUN_DIALOG"

-- | Launch a panel action. Either the "Run Application" dialog ("run_dialog" parameter,
-- see above) or the main menu ("main_menu" parameter).  mate-panel must be running
-- for this to work.
matePanel :: String -> X ()
matePanel action = withDisplay $ \dpy -> do
    let panel = "_MATE_PANEL_ACTION"
    rw <- asks theRoot
    mate_panel <- getAtom panel
    panel_action <- getAtom (panel ++ "_" ++ map toUpper action)

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_action 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

-- | Register xmonad with mate. 'dbus-send' must be in the $PATH with which
-- xmonad is started.
--
-- This action reduces a delay on startup only if you have configured
-- mate-session to start xmonad with a command such as (check local
-- documentation):
--
-- > dconf write /org/mate/desktop/session/required_components/windowmanager "'xmonad'"
--
-- (the extra quotes are required by dconf)
mateRegister :: MonadIO m => m ()
mateRegister = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" <$> getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.mate.SessionManager"
            ,"/org/mate/SessionManager"
            ,"org.mate.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]

-- | Display MATE logout dialog. This is the default mod-q action.
mateLogout :: MonadIO m => m ()
mateLogout = spawn "mate-session-save --logout-dialog"

-- | Display MATE shutdown dialog. You can override mod-q to invoke this, or bind it
-- to another key if you prefer.
mateShutdown :: MonadIO m => m ()
mateShutdown = spawn "mate-session-save --shutdown-dialog"
