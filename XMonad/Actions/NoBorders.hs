-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.NoBorders
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Lukas Mai <l.mai@web.de>
-- Stability   :  unstable
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
-- > , ((modMask x,  xK_g ),   withFocused toggleBorder)
--
toggleBorder :: Window -> X ()
toggleBorder w = do
    bw <- asks (borderWidth . config)
    withDisplay $ \d -> io $ do
        cw <- wa_border_width `fmap` getWindowAttributes d w
        if cw == 0
            then setWindowBorderWidth d w bw
            else setWindowBorderWidth d w 0
