-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.ManageDocks
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes xmonad detect windows with type DOCK and does not put them in
-- layouts.
-----------------------------------------------------------------------------
module XMonadContrib.ManageDocks (
	-- * Usage
	-- $usage
	manageDocksHook
	) where

import Control.Monad.Reader
import XMonad
import Operations
import qualified StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- Add the imports to your configuration file and add the mangeHook:
--
-- > import XMonadContrib.ManageDocks
--
-- > manageHook w _ _ _  = manageDocksHook w

-- %import XMonadContrib.ManageDocks
-- %def -- comment out default manageHook definition above if you uncomment this:
-- %def manageHook _ _ _ = manageDocksHook w


-- |
-- Deteckts if the given window is of type DOCK and if so, reveals it, but does
-- not manage it
manageDocksHook :: Window -> X (WindowSet -> WindowSet)
manageDocksHook w = do
	isDock <- checkDock w
	if isDock then do
		reveal w
		return (W.delete w)
	      else do
	        return id

checkDock :: Window -> X (Bool)
checkDock w = do
	a <- getAtom "_NET_WM_WINDOW_TYPE"
	d <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
	mbr <- withDisplay $ \dpy -> do
		io $ getWindowProperty32 dpy a w 
	case mbr of 
		Just [r] -> return (r == d)
		_        -> return False
