{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -fglasgow-exts #-}
-- deriving Typeable
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageDocks
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides tools to automatically manage 'dock' type programs,
-- such as gnome-panel, kicker, dzen, and xmobar.

module XMonad.Hooks.ManageDocks (
    -- * Usage
    -- $usage
    manageDocks, AvoidStruts, avoidStruts, ToggleStruts(ToggleStruts)
    ) where


-----------------------------------------------------------------------------
import XMonad
import Foreign.C.Types (CLong)
import Data.Maybe (catMaybes)
-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ManageDocks
--
-- The first component is a 'ManageHook' which recognizes these windows.  To
-- enable it:
--
-- > manageHook = ... <+> manageDocks
--
-- The second component is a layout modifier that prevents windows from
-- overlapping these dock windows.  It is intended to replace xmonad's
-- so-called "gap" support.  First, you must add it to your list of layouts:
--
-- > layoutHook = avoidStruts (tall ||| mirror tall ||| ...)
--
-- 'AvoidStruts' also supports toggling the dock gap, add a keybinding similar
-- to:
--
-- > ,((modMask,               xK_b     ), sendMessage ToggleStruts)
--

-- |
-- Detects if the given window is of type DOCK and if so, reveals it, but does
-- not manage it. If the window has the STRUT property set, adjust the gap accordingly.
manageDocks :: ManageHook
manageDocks = checkDock --> doIgnore

-- |
-- Checks if a window is a DOCK window
checkDock :: Query Bool
checkDock = ask >>= \w -> liftX $ do
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
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- |
-- Goes through the list of windows and find the gap so that all STRUT
-- settings are satisfied.
calcGap :: X Rectangle
calcGap = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    -- We don't keep track of dock like windows, so we find all of them here
    (_,_,wins) <- io $ queryTree dpy rootw
    struts <- catMaybes `fmap` mapM getStrut wins

    -- we grab the window attributes of the root window rather than checking
    -- the width of the screen because xlib caches this info and it tends to
    -- be incorrect after RAndR
    wa <- io $ getWindowAttributes dpy rootw
    return $ reduceScreen (foldl max4 (0,0,0,0) struts)
                $ Rectangle (fi $ wa_x wa) (fi $ wa_y wa) (fi $ wa_width wa) (fi $ wa_height wa)

-- |
-- Piecewise maximum of a 4-tuple of Ints
max4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
max4 (a1,a2,a3,a4) (b1,b2,b3,b4) = (max a1 b1, max a2 b2, max a3 b3, max a4 b4)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Given strut values and the screen rectangle, compute a reduced screen
-- rectangle.
reduceScreen :: (Int, Int, Int, Int) -> Rectangle -> Rectangle
reduceScreen (t, b, l, r) s
 = case r2c s of
    (x1, y1, x2, y2) -> c2r (x1 + fi l, y1 + fi t, x2 - fi r, y2 - fi b)

r2c :: Rectangle -> (Position, Position, Position, Position)
r2c (Rectangle x y w h) = (x, y, x + fi w, y + fi h)

c2r :: (Position, Position, Position, Position) -> Rectangle
c2r (x1, y1, x2, y2) = Rectangle x1 y1 (fi $ x2 - x1) (fi $ y2 - y1)

-- | Given a bounding rectangle 's' and another rectangle 'r', compute a
-- rectangle 'r' that fits inside 's'.
fitRect :: Rectangle -> Rectangle -> Rectangle
fitRect s r
 = c2r (max sx1 rx1, max sy1 ry1, min sx2 rx2, min sy2 ry2)
 where
    (sx1, sy1, sx2, sy2) = r2c s
    (rx1, ry1, rx2, ry2) = r2c r

-- | Adjust layout automagically.
avoidStruts :: LayoutClass l a => l a -> AvoidStruts l a
avoidStruts = AvoidStruts True

data AvoidStruts l a = AvoidStruts Bool (l a) deriving ( Read, Show )

data ToggleStruts = ToggleStruts deriving (Read,Show,Typeable)
instance Message ToggleStruts

instance LayoutClass l a => LayoutClass (AvoidStruts l) a where
    doLayout (AvoidStruts True lo) r s =
        do rect <- fmap (flip fitRect r) calcGap
           (wrs,mlo') <- doLayout lo rect s
           return (wrs, AvoidStruts True `fmap` mlo')
    doLayout (AvoidStruts False lo) r s = do (wrs,mlo') <- doLayout lo r s
                                             return (wrs, AvoidStruts False `fmap` mlo')
    handleMessage (AvoidStruts b l) m
        | Just ToggleStruts <- fromMessage m = return $ Just $ AvoidStruts (not b) l
        | otherwise = do ml' <- handleMessage l m
                         return (AvoidStruts b `fmap` ml')
    description (AvoidStruts _ l) = description l
