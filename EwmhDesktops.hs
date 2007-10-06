-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.EwmhDesktops
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes xmonad use the EWMH hints to tell panel applications about its
-- workspaces and the windows therein.
-----------------------------------------------------------------------------
module XMonadContrib.EwmhDesktops (
	-- * Usage
	-- $usage
	ewmhDesktopsLogHook
	) where

import Data.List	(elemIndex, sortBy)
import Data.Ord 	(comparing)
import Data.Maybe	(fromMaybe)

import Control.Monad.Reader
import XMonad
import qualified StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- Add the imports to your configuration file and add the logHook:
--
-- > import XMonadContrib.EwmhDesktops
--
-- > logHook :: X()
-- > logHook = do ewmhDesktopsLogHook
-- >              return ()

-- %import XMonadContrib.EwmhDesktops
-- %def -- comment out default logHook definition above if you uncomment this:
-- %def logHook = ewmhDesktopsLogHook


-- |
-- Notifies pagers and window lists, such as those in the gnome-panel
-- of the current state of workspaces and windows.
ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = withWindowSet $ \s -> do
	-- Bad hack because xmonad forgets the original order of things, it seems
	-- see http://code.google.com/p/xmonad/issues/detail?id=53
	let ws = sortBy (comparing W.tag) $ W.workspaces s
	let wins = W.allWindows s

	-- Number of Workspaces
	setNumberOfDesktops (length ws)

	-- Names thereof
	setDesktopNames (map W.tag ws)
	
	-- Current desktop
	fromMaybe (return ()) $ do
		n <- W.lookupWorkspace 0 s
		i <- elemIndex n $ map W.tag ws
		return $ setCurrentDesktop i

	setClientList wins

	-- Per window Desktop
	forM (zip ws [(0::Int)..]) $ \(w, wn) ->
		forM (W.integrate' (W.stack w)) $ \win -> do 
			setWindowDesktop win wn
	
	return ()


setNumberOfDesktops :: (Integral a) => a -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do 
        a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
        c <- getAtom "CARDINAL"
	r <- asks theRoot
	io $ changeProperty32 dpy r a c propModeReplace [fromIntegral n]

setCurrentDesktop :: (Integral a) => a -> X ()
setCurrentDesktop i = withDisplay $ \dpy -> do
	a <- getAtom "_NET_CURRENT_DESKTOP"
	c <- getAtom "CARDINAL"
	r <- asks theRoot
	io $ changeProperty32 dpy r a c propModeReplace [fromIntegral i]

setDesktopNames :: [String] -> X ()
setDesktopNames names = withDisplay $ \dpy -> do
	-- Names thereof
	r <- asks theRoot
	a <- getAtom "_NET_DESKTOP_NAMES"
	c <- getAtom "UTF8_STRING"
	let names' = map (fromIntegral.fromEnum) $
			concatMap (("Workspace "++) . (++['\0'])) names
	io $ changeProperty8 dpy r a c propModeReplace names'

setClientList :: [Window] -> X ()
setClientList wins = withDisplay $ \dpy -> do
	-- (What order do we really need? Something about age and stacking)
	r <- asks theRoot
	c <- getAtom "WINDOW"
	a <- getAtom "_NET_CLIENT_LIST"
	io $ changeProperty32 dpy r a c propModeReplace wins
	a' <- getAtom "_NET_CLIENT_LIST_STACKING"
	io $ changeProperty32 dpy r a' c propModeReplace wins

setWindowDesktop :: (Integral a) => Window -> a -> X ()
setWindowDesktop win i = withDisplay $ \dpy -> do 
	a <- getAtom "_NET_WM_DESKTOP"
	c <- getAtom "CARDINAL"
	io $ changeProperty32 dpy win a c propModeReplace [fromIntegral i]
