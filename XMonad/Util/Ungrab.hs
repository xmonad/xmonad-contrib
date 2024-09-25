{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Ungrab
-- Description :  Release xmonad's keyboard and pointer grabs immediately.
-- Copyright   :  (c) 2016 Brandon S Allbery
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Allow releasing xmonad's keyboard grab
--
-----------------------------------------------------------------------------

module XMonad.Util.Ungrab {-# DEPRECATED "Use XMonad.Operations.unGrab instead" #-}
    ( -- * Usage:
      -- $usage
      unGrab
    ) where

#if MIN_VERSION_xmonad(0, 17, 9)
import XMonad.Operations (unGrab)
#else
import Graphics.X11.Xlib (sync)
import Graphics.X11.Xlib.Extras (currentTime)
import Graphics.X11.Xlib.Misc (ungrabKeyboard, ungrabPointer)
import XMonad.Core

-- $usage
-- Start a keyboard action with this if it is going to run something
-- that needs to do a keyboard, pointer, or server grab. For example,
--
-- > , ((modm .|. controlMask, xK_p), unGrab >> spawn "scrot")
--
-- (Other examples are screen lockers and "gksu".)
-- This avoids needing to insert a pause/sleep before running the
-- command.
--
-- xmonad retains the keyboard grab during key actions because if they
-- use a Submap, they need the keyboard to be grabbed, and if they had
-- to assert their own grab then the asynchronous nature of X11 allows
-- race conditions between xmonad, other clients, and the X server that
-- would cause keys to sometimes be "leaked" to the focused window.

-- | Release xmonad's keyboard grab, so other grabbers can do their thing.
unGrab :: X ()
unGrab = withDisplay $ \d -> io (ungrabKeyboard d currentTime >> ungrabPointer d currentTime >> sync d False)
#endif
