module XMonadContrib.EwmhDesktops (ewmhDesktopsLogHook) where

import Data.Maybe   (listToMaybe,fromJust)
import Data.List  (elemIndex, sortBy)
import Data.Ord ( comparing)

import Control.Monad.Reader
import XMonad
import qualified StackSet as W
import System.IO
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = withDisplay $ \dpy -> withWindowSet $ \s -> do
	-- Number of Workspaces
	-- Bad hack because xmonad forgets the original order of things, it seems
	let ws = sortBy (comparing W.tag) $ W.workspaces s

	let n = fromIntegral (length ws)
        a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
        c <- getAtom "CARDINAL"
	r <- asks theRoot
	io $ changeProperty32 dpy r a c propModeReplace [n]

	-- Names thereof
	a <- getAtom "_NET_DESKTOP_NAMES"
	c <- getAtom "UTF8_STRING"
	let names = map (fromIntegral.fromEnum) $
			concatMap (("Workspace "++) . (++['\0']). W.tag) ws
	io $ changeProperty8 dpy r a c propModeReplace names
	
	-- Current desktop
	a <- getAtom "_NET_CURRENT_DESKTOP"
	c <- getAtom "CARDINAL"
	let Just n = W.lookupWorkspace 0 s
	let Just i = elemIndex n $ map W.tag ws
	io $ changeProperty32 dpy r a c propModeReplace [fromIntegral i]
	
	return ()


