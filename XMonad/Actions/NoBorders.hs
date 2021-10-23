-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.NoBorders
-- Description :  Helper functions for dealing with window borders.
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Lukas Mai <l.mai@web.de>
-- Stability   :  stable
-- Portability :  unportable
--
-- This module provides helper functions for dealing with window borders.
--
-----------------------------------------------------------------------------

module XMonad.Actions.NoBorders (
    toggleBorder
) where

import XMonad

-- | Toggle the border of the currently focused window. To use it, add a
-- keybinding like so:
--
-- > , ((modm,  xK_g ),   withFocused toggleBorder)
--
toggleBorder :: Window -> X ()
toggleBorder w = do
    bw <- asks (borderWidth . config)
    withDisplay $ \d -> withWindowAttributes d w $ \wa -> io $
        if wa_border_width wa == 0
            then setWindowBorderWidth d w bw
            else setWindowBorderWidth d w 0
