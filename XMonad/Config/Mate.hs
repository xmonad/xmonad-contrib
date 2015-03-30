{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Mate
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
    mateRegister
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
-- > import XMonad.Config.Mate
-- >
-- > main = xmonad mateConfig
--
-- For examples of how to further customize @mateConfig@ see "XMonad.Config.Desktop".

mateConfig = desktopConfig
    { terminal = "mate-terminal"
    , keys     = mateKeys <+> keys desktopConfig
    , startupHook = mateRegister >> startupHook desktopConfig }

mateKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), mateRun)
    , ((modm .|. shiftMask, xK_q), spawn "mate-session-save --logout-dialog") ]

-- | Launch the "Run Application" dialog.  mate-panel must be running for this
-- to work.
mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run  <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
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
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.mate.SessionManager"
            ,"/org/mate/SessionManager"
            ,"org.mate.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]
