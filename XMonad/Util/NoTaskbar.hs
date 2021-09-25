-----------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.NoTaskbar
-- Description : Mark a window to be ignored by EWMH taskbars and pagers.
-- Copyright   : (c) ???
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : ???
--
-- Function and manageHook to mark a window to be ignored by EWMH
-- taskbars and pagers.
--
-----------------------------------------------------------------------------

module XMonad.Util.NoTaskbar (-- * Usage
                              -- $usage
                              noTaskbar
                             ,markNoTaskbar) where

import XMonad.Core
import XMonad.Prelude (fi)
import XMonad.ManageHook
import Graphics.X11.Xlib (Window)
import Graphics.X11.Xlib.Atom (aTOM)
import Graphics.X11.Xlib.Extras (changeProperty32
                                ,propModePrepend)
import Control.Monad.Reader (ask)

-- $usage
-- Utility functions to hide windows from pagers and taskbars. Mostly useful
-- when EWMH doesn't do what you intend (e.g. for 'NamedScratchpad' windows you
-- probably don't want to be dumped into the 'NSP' workspace).

-- | A 'ManageHook' to mark a window to not be shown in pagers or taskbars.
noTaskbar :: ManageHook
noTaskbar = ask >>= (>> idHook) . liftX . markNoTaskbar

-- | An 'X' action to mark a window to not be shown in pagers or taskbars.
markNoTaskbar :: Window -> X ()
markNoTaskbar w = withDisplay $ \d -> do
                    ws <- getAtom "_NET_WM_STATE"
                    ntb <- getAtom "_NET_WM_STATE_SKIP_TASKBAR"
                    npg <- getAtom "_NET_WM_STATE_SKIP_PAGER"
                    io $ changeProperty32 d w ws aTOM propModePrepend [fi ntb,fi npg]
