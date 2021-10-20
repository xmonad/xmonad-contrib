{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Config.Desktop
-- Description  : Core settings for interfacing with desktop environments.
-- Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License      : BSD
--
-- Maintainer   : Spencer Janssen <spencerjanssen@gmail.com>
-- Stability    :  unstable
-- Portability  :  unportable
--
-- This module provides a config suitable for use with a desktop
-- environment such as KDE or GNOME.
-----------------------------------------------------------------------------
module XMonad.Config.Desktop (

    -- | Several basic integration settings are common to all of xmonad's
    -- desktop integration configurations. The specific desktop environment
    -- (DE) modules like "XMonad.Config.Gnome" use this module's
    -- @desktopConfig@ to set up basic communication between xmonad and
    -- the DE via a subset of the Extended Window Manager Hints (EWMH)
    -- specification. Extra xmonad settings unique to specific DE's are
    -- added by overriding or modifying @desktopConfig@ fields in the
    -- same way that the default configuration is customized in
    -- @~\/.xmonad/xmonad.hs@.
    --
    -- For more information about EWMH see:
    --
    -- <http://standards.freedesktop.org/wm-spec/wm-spec-latest.html>
    --
    -- See also: "XMonad.Hooks.EwmhDesktops", "XMonad.Hooks.ManageDocks",
    -- "XMonad.Util.EZConfig".

    -- * Usage
    -- $usage

    desktopConfig,

    -- * Customizing a desktop config
    -- $customizing

    -- ** Modifying layouts, manageHook, or key bindings
    -- $layouts
    desktopLayoutModifiers

    -- ** Modifying the logHook
    -- $logHook

    -- ** Modifying the handleEventHook
    -- $eventHook

    -- ** Modifying the startupHook
    -- $startupHook
    ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.Cursor

import qualified Data.Map as M

-- $usage
-- While this document describes how to configure xmonad, you also need
-- to set up your Desktop Environment (DE) and display manager to use
-- xmonad as its window manager. For DE and distro specific tips on
-- how to do so, see the xmonad wiki:
--
-- <http://haskell.org/haskellwiki/Xmonad>
--
-- To configure xmonad for use with a DE or with DE tools like panels
-- and pagers, in place of @def@ in your @~\/.xmonad/xmonad.hs@,
-- use @desktopConfig@ or one of the other desktop configs from the
-- @XMonad.Config@ namespace. The following setup and customization examples
-- work the same way for the other desktop configs as for @desktopConfig@.
-- If you are using a specific DE config, import its module instead, and
-- use its config in place of @desktopConfig@ in the following examples.
--
-- > import XMonad
-- > import XMonad.Config.Desktop
-- >
-- > main = xmonad desktopConfig
--
-- @desktopConfig@ is an 'XConfig' that configures xmonad to
-- ignore and leave room for dock type windows like panels and trays, adds
-- the default key binding to toggle panel visibility, and activates basic
-- EWMH support. It also sets a prettier root window mouse pointer.

-- $customizing
-- To customize a desktop config, modify its fields as is illustrated with
-- the default configuration @def@ in "XMonad.Doc.Extending#Extending xmonad".

-- $layouts
-- See also "XMonad.Util.EZConfig" for more options for modifying key bindings.
-- To add to layouts, manageHook or key bindings use something like the following
-- to combine your modifications with the desktop config settings:
--
-- > import XMonad
-- > import XMonad.Config.Desktop
-- > import XMonad.Layout.Tabbed
-- > import XMonad.Util.EZConfig (additionalKeys)
-- >
-- > main =
-- >   xmonad $ desktopConfig {
-- >     -- add manage hooks while still ignoring panels and using default manageHooks
-- >       manageHook = myManageHook <+> manageHook desktopConfig
-- >
-- >     -- add a fullscreen tabbed layout that does not avoid covering
-- >     -- up desktop panels before the desktop layouts
-- >     , layoutHook = simpleTabbed ||| layoutHook desktopConfig
-- >     }
-- >     -- add a screenshot key to the default desktop bindings
-- >     `additionalKeys` [ ((mod4Mask, xK_F8), spawn "scrot") ]
--
-- To replace the desktop layouts with your own choices, but still
-- allow toggling panel visibility, use 'desktopLayoutModifiers' to
-- modify your layouts:
--
-- >  , layoutHook = desktopLayoutModifiers $ simpleTabbed ||| Tall 1 0.03 0.5
--
-- @desktopLayoutModifiers@ modifies a layout to avoid covering docks, panels,
-- etc. that set the @_NET_WM_STRUT_PARTIAL@ property.
-- See also "XMonad.Hooks.ManageDocks".

-- $logHook
-- To add to the logHook while still sending workspace and window information
-- to DE apps use something like:
--
-- >  , logHook = myLogHook <+> logHook desktopConfig
--
-- Or for more elaborate logHooks you can use @do@:
--
-- >  , logHook = do
-- >        dynamicLogWithPP xmobarPP
-- >        updatePointer (Relative 0.9 0.9)
-- >        logHook desktopConfig
--

-- $eventHook
-- To customize xmonad's event handling while still having it respond
-- to EWMH events from pagers, task bars:
--
-- >  , handleEventHook = myEventHooks <+> handleEventHook desktopConfig
--
-- or 'mconcat' if you write a list event of event hooks
--
-- >  , handleEventHook = mconcat
-- >        [ myMouseHandler
-- >        , myMessageHandler
-- >        , handleEventHook desktopConfig ]
--
-- Note that the event hooks are run left to right (in contrast to
-- 'ManageHook'S which are right to left)

-- $startupHook
-- To run the desktop startupHook, plus add further actions to be run each
-- time xmonad starts or restarts, use '<+>' to combine actions as in the
-- logHook example, or something like:
--
-- >  , startupHook = do
-- >        startupHook desktopConfig
-- >        spawn "xmonad-restart.sh"
-- >        adjustEventInput
--

desktopConfig :: XConfig (ModifiedLayout AvoidStruts
                             (Choose Tall (Choose (Mirror Tall) Full)))
desktopConfig = docks $ ewmh def
    { startupHook     = setDefaultCursor xC_left_ptr <+> startupHook def
    , layoutHook      = desktopLayoutModifiers $ layoutHook def
    , keys            = desktopKeys <+> keys def }

desktopKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
desktopKeys XConfig{modMask = modm} = M.fromList
    [ ((modm, xK_b), sendMessage ToggleStruts) ]

desktopLayoutModifiers :: LayoutClass l a => l a -> ModifiedLayout AvoidStruts l a
desktopLayoutModifiers = avoidStruts
