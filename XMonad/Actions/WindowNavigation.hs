-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowNavigation
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>,
--                          Devin Mullins <me@twifkak.com>
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- License     :  BSD3-style (see LICENSE)
--
-- This is a rewrite of "XMonad.Layout.WindowNavigation", for the purposes of
-- code cleanup and Xinerama support. It's not complete, so you'll want to
-- use that one for now.
--
-- WindowNavigation lets you assign keys to move up/down/left/right, based on
-- actual window geometry, rather than just going j/k on the stack.
--
-----------------------------------------------------------------------------

module XMonad.Actions.WindowNavigation (
                                       -- * Usage
                                       -- $usage
                                       go, swap,
                                       Direction(..)
                                       ) where

import XMonad
import XMonad.Hooks.ManageDocks (Direction(..))
import qualified XMonad.StackSet as W

import Control.Applicative ((<$>))
import Data.IORef
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Graphics.X11.Xlib

-- $usage
--
-- Don't use it! What, are you crazy?

-- TODO: IORef should be a map from WorkspaceId to Point
-- TODO: solve the 2+3, middle right to bottom left problem
--       logHook to update currentPosition?

-- go:
-- 1. get current position, verifying it matches the current window
-- 2. get target windowrect
-- 3. focus window
-- 4. set new position

-- key bindings to do the important stuff

-- 1. Get current position, window
-- 2. Determine list of windows in dir from pos, except window
-- 3. Grab closest one

go :: IORef (Maybe Point) -> Direction -> X ()
go posRef dir = fromCurrentPoint $ \win pos -> do
    targets <- filter ((/= win) . fst) <$> navigableTargets pos dir
    io $ putStrLn $ "pos: " ++ show pos ++ "; tgts: " ++ show targets
    whenJust (listToMaybe targets) $ \(tw, tr) -> do
      windows (W.focusWindow tw)
      setPosition posRef pos tr
  where fromCurrentPoint f = withFocused $ \win -> do
                                 currentPosition posRef >>= f win

swap :: IORef (Maybe Point) -> Direction -> X ()
swap _ _ = return ()

-- Gets the current position from the IORef passed in, or if nothing (say, from
-- a restart), derives the current position from the current window. Also,
-- verifies that the position is congruent with the current window (say, if you
-- used mod-j/k or mouse or something).
-- TODO: replace 0 0 0 0 with 'middle of current window'
-- TODO: correct if not in window, or add logHook
currentPosition :: IORef (Maybe Point) -> X Point
currentPosition posRef = do
    mp <- io $ readIORef posRef
    return $ fromMaybe (Point 0 0) mp

navigableTargets :: Point -> Direction -> X [(Window, Rectangle)]
navigableTargets point dir = navigable dir point <$> windowRects

setPosition :: IORef (Maybe Point) -> Point -> Rectangle -> X ()
setPosition posRef _ (Rectangle x y w h) =
    let position = Point (x + (fromIntegral w `div` 2)) (y + (fromIntegral h `div` 2)) in
    io $ writeIORef posRef (Just position)

-- Filters and sorts the windows in terms of what is closest from the Point in
-- the Direction.
navigable :: Direction -> Point -> [(Window, Rectangle)] -> [(Window, Rectangle)]
navigable d pt = sortby d . filter (inr d (fromPoint pt) . snd)

-- Produces a list of normal-state windows, on any screen. Rectangles are
-- adjusted based on screen position relative to the current screen, because I'm
-- bad like that.
-- TODO: only the visible windows
-- TODO: adjust rectangles based on screen position :P
windowRects :: X [(Window, Rectangle)]
windowRects = do
    dpy <- asks display
    wins <- gets (visibleWindows . windowset)
    catMaybes <$> mapM (windowRect dpy) wins
  where visibleWindows wset = concatMap (W.integrate' . W.stack . W.workspace)
                                        (W.current wset : W.visible wset)

windowRect :: Display -> Window -> X (Maybe (Window, Rectangle))
windowRect dpy win = do
    (_, x, y, w, h, _, _) <- io $ getGeometry dpy win
    return $ Just $ (win, Rectangle x y w h)
    `catchX` return Nothing

-- manageHook to draw window decos?

fromPoint :: Point -> FPoint
fromPoint p = P (fromIntegral $ pt_x p) (fromIntegral $ pt_y p)

-- Stolen from droundy's implementation of WindowNavigation. I should probably take the time
-- to understand the black magic below at some point.

data FPoint = P Double Double

inr :: Direction -> FPoint -> Rectangle -> Bool
inr D (P x y) (Rectangle l yr w h) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y <  fromIntegral yr + fromIntegral h
inr U (P x y) (Rectangle l yr w _) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y >  fromIntegral yr
inr R (P a x) (Rectangle b l _ w)  = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     a <  fromIntegral b
inr L (P a x) (Rectangle b l c w)  = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     a >  fromIntegral b + fromIntegral c

sortby :: Direction -> [(a,Rectangle)] -> [(a,Rectangle)]
sortby U = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y' y)
sortby D = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y y')
sortby R = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x x')
sortby L = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x' x)
