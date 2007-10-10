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
-- layouts. It also detects window with STRUT set and modifies the
-- gap accordingly.
--
-- Cheveats:
--
--  * Only acts on STRUT apps on creation, not if you move or close them
--
--  * To reset the gap, press Mod-b twice and restart xmonad (Mod-q)
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
import Data.Word

-- $usage
-- Add the imports to your configuration file and add the mangeHook:
--
-- > import XMonadContrib.ManageDocks
--
-- > manageHook w _ _ _  = manageDocksHook w
--
-- and comment out the default `manageHook _ _ _ _ = return id` line.

-- %import XMonadContrib.ManageDocks
-- %def -- comment out default manageHook definition above if you uncomment this:
-- %def manageHook w _ _ _ = manageDocksHook w


-- |
-- Detects if the given window is of type DOCK and if so, reveals it, but does
-- not manage it. If the window has the STRUT property set, adjust the gap accordingly.
manageDocksHook :: Window -> X (WindowSet -> WindowSet)
manageDocksHook w = do
    hasStrut <- getStrut w
    maybe (return ()) setGap hasStrut

    isDock <- checkDock w
    if isDock then do
        reveal w
        return (W.delete w)
          else do
            return id

-- |
-- Checks if a window is a DOCK window
checkDock :: Window -> X (Bool)
checkDock w = do
    a <- getAtom "_NET_WM_WINDOW_TYPE"
    d <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
    mbr <- getProp a w
    case mbr of
        Just [r] -> return (fromIntegral r == d)
        _        -> return False

-- |
-- Gets the STRUT config, if present, in xmonad gap order
getStrut :: Window -> X (Maybe (Int, Int, Int, Int))
getStrut w = do
    a <- getAtom "_NET_WM_STRUT"
    mbr <- getProp a w
    case mbr of
        Just [l,r,t,b] -> return (Just (
                    fromIntegral t,
                    fromIntegral b,
                    fromIntegral l,
                    fromIntegral r))
        _              -> return Nothing

-- |
-- Helper to read a property
getProp :: Atom -> Window -> X (Maybe [Word32])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- |
-- Modifies the gap, setting new max
setGap :: (Int, Int, Int, Int) -> X ()
setGap gap = modifyGap (\_ -> max4 gap)

-- |
-- Piecewise maximum of a 4-tuple of Ints
max4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
max4 (a1,a2,a3,a4) (b1,b2,b3,b4) = (max a1 b1, max a2 b2, max a3 b3, max a4 b4)
