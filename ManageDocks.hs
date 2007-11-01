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
-- It also allows you to reset the gap to reflect the state of current STRUT
-- windows (for example, after you resized or closed a panel), and to toggle the Gap
-- in a STRUT-aware fashion.
-----------------------------------------------------------------------------
module XMonadContrib.ManageDocks (
    -- * Usage
    -- $usage
    manageDocksHook
    ,resetGap
    ,toggleGap
    ,avoidStruts
    ) where

import Control.Monad.Reader
import XMonad
import XMonad.Operations
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Data.Word (Word32)
import Data.Maybe (catMaybes)

-- $usage
-- Add the imports to your configuration file and add the mangeHook:
--
-- > import XMonadContrib.ManageDocks
--
-- > manageHook w _ _ _  = manageDocksHook w
--
-- and comment out the default `manageHook _ _ _ _ = return id` line.
--
-- Then you can bind resetGap or toggleGap as you wish:
--
-- > , ((modMask,               xK_b), toggleGap)

-- %import XMonadContrib.ManageDocks
-- %def -- comment out default manageHook definition above if you uncomment this:
-- %def manageHook w _ _ _ = manageDocksHook w
-- %keybind , ((modMask,               xK_b), toggleGap)


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
-- Goes through the list of windows and find the gap so that all STRUT
-- settings are satisfied.
calcGap :: X (Int, Int, Int, Int)
calcGap = withDisplay $ \dpy -> do
	rootw <- asks theRoot
	-- We don’t keep track of dock like windows, so we find all of them here
	(_,_,wins) <- io $ queryTree dpy rootw
	struts <- catMaybes `fmap` mapM getStrut wins
	return $ foldl max4 (0,0,0,0) struts

-- |
-- Adjusts the gap to the STRUTs of all current Windows 
resetGap :: X ()
resetGap = do
	newGap <- calcGap
	modifyGap (\_ _ -> newGap)

-- |
-- Removes the gap or, if already removed, sets the gap according to the windows’ STRUT
toggleGap :: X ()
toggleGap = do
	newGap <- calcGap
	modifyGap (\_ old -> if old == (0,0,0,0) then newGap else (0,0,0,0))

-- |
-- Piecewise maximum of a 4-tuple of Ints
max4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
max4 (a1,a2,a3,a4) (b1,b2,b3,b4) = (max a1 b1, max a2 b2, max a3 b3, max a4 b4)

-- | Adjust layout automagically.
avoidStruts :: LayoutClass l a => l a -> AvoidStruts l a
avoidStruts = AvoidStruts

data AvoidStruts l a = AvoidStruts (l a) deriving ( Read, Show )

instance LayoutClass l a => LayoutClass (AvoidStruts l) a where
    doLayout (AvoidStruts lo) (Rectangle x y w h) s =
        do (t,l,b,r) <- calcGap
           let rect = Rectangle (x+10+fromIntegral l) (y+fromIntegral t)
                      (w-fromIntegral l-fromIntegral r) (h-fromIntegral t-fromIntegral b)
           (wrs,mlo') <- doLayout lo rect s
           return (wrs, AvoidStruts `fmap` mlo')
    handleMessage (AvoidStruts l) m =
        do ml' <- handleMessage l m
           return (AvoidStruts `fmap` ml')
    description (AvoidStruts l) = description l
