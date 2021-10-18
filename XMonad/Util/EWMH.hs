{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  XMonad.Util.EWMH
-- Description :  Utilities for modules implementing Extended Window Manager Hints (EWMH).
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- The common bits of of xmonad's implementation of the EWMH specification
-- (<https://specifications.freedesktop.org/wm-spec/latest/>).
--
module XMonad.Util.EWMH (
    -- * Usage
    -- $usage

    -- * @_NET_SUPPORTED@ abstraction
    ewmhSupported,
    ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleConf as XC

-- ---------------------------------------------------------------------
-- $usage
--
-- This module is not intended to be used in user configurations.
--
-- Contrib modules implementing parts of the EWMH specification should export
-- an 'XConfig' combinator which applies 'ewmhSupported' to advertise the
-- hints it implements, and uses 'XMonad.Util.ExtensibleConf.once' to attach
-- its hooks to the user's 'XConfig'.
--
-- A very simple example:
--
-- > import XMonad.Util.EWMH
-- > import qualified XMonad.Util.ExtensibleConf as XC
-- >
-- > data EwmhDesktopsConfig = EwmhDesktopsConfig{…}
-- > instance Semigroup EwmhDesktopsConfig where …
-- >
-- > ewmhDesktops :: EwmhDesktopsConfig -> XConfig a -> XConfig a
-- > ewmhDesktops = ewmhSupported hints .: XC.once hooks
-- >   where
-- >     hints = ["_NET_CURRENT_DESKTOP", "_NET_NUMBER_OF_DESKTOPS", "_NET_DESKTOP_NAMES", "_NET_WM_DESKTOP"]
-- >     hooks c = c{ handleEventHook = handleEventHook c <> ewmhDesktopsEventHook }

-- ---------------------------------------------------------------------
-- @_NET_SUPPORTED@ abstraction

newtype EwmhSupported = EwmhSupported{ getSupported :: [String] } deriving (Semigroup)

-- | Add given atoms to the @_NET_SUPPORTED@ list of supported hints.
--
-- The property is set once, as the very first 'startupHook' when xmonad
-- starts.
ewmhSupported :: [String] -> XConfig l -> XConfig l
ewmhSupported = XC.once (\c -> c{ startupHook = setSupported <> startupHook c }) . EwmhSupported

setSupported :: X ()
setSupported = XC.with $ \supported ->
    withDisplay $ \dpy -> do
        r <- asks theRoot
        a <- getAtom "_NET_SUPPORTED"
        atoms <- mapM getAtom $ nub $ getSupported supported
        io $ changeProperty32 dpy r a aTOM propModeReplace $ map fi atoms
