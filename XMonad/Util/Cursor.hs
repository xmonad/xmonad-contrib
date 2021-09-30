----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Cursor
-- Description :  Set the default mouse cursor.
-- Copyright   :  (c) 2009 Collabora Ltd
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Andres Salomon <dilinger@collabora.co.uk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for setting the default mouse cursor.
--
-- Some ideas shamelessly stolen from Nils Schweinsberg; thanks!
-----------------------------------------------------------------------------

module XMonad.Util.Cursor
    ( -- * Usage:
      -- $usage
      module Graphics.X11.Xlib.Cursor,
      setDefaultCursor
    ) where

import Graphics.X11.Xlib.Cursor
import XMonad

-- $usage
--
-- >   setDefaultCursor xC_left_ptr
--
--   For example, to override the default gnome cursor:
--
-- >   import XMonad.Util.Cursor
-- >   main = xmonad gnomeConfig { startupHook = setDefaultCursor xC_pirate }
--
--   Arrr!

-- | Set the default (root) cursor
setDefaultCursor :: Glyph -> X ()
setDefaultCursor glyph = do
    dpy <- asks display
    rootw <- asks theRoot
    liftIO $ do
        curs <- createFontCursor dpy glyph
        defineCursor dpy rootw curs
        flush dpy
        freeCursor dpy curs
