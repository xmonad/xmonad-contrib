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
-- actual cartesian window coordinates, rather than just going j/k on the stack.
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
import Data.Map (Map())
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Graphics.X11.Xlib

-- $usage
--
-- Don't use it! What, are you crazy?

-- TODO:
--  - withWindowNavigation :: XConfig l -> XConfig l
--  - cleanup
--  - actually deal with multiple screens
--  - documentation :)
--  - tests? (esp. for edge cases in currentPosition)
--  - solve the 2+3, middle right to bottom left problem
--  - manageHook to draw window decos?


type WNState = Map WorkspaceId Point

-- go:
-- 1. get current position, verifying it matches the current window
-- 2. get target windowrect
-- 3. focus window
-- 4. set new position
go :: IORef WNState -> Direction -> X ()
go posRef dir = fromCurrentPoint $ \win pos -> do
    targets <- filter ((/= win) . fst) <$> navigableTargets pos dir
    whenJust (listToMaybe targets) $ \(tw, tr) -> do
      windows (W.focusWindow tw)
      setPosition posRef pos tr
  where fromCurrentPoint f = withFocused $ \win -> do
                                 currentPosition posRef >>= f win

swap :: IORef WNState -> Direction -> X ()
swap _ _ = return ()

-- Gets the current position from the IORef passed in, or if nothing (say, from
-- a restart), derives the current position from the current window. Also,
-- verifies that the position is congruent with the current window (say, if you
-- used mod-j/k or mouse or something).
-- TODO: worry about off-by-one issues with inside definition
currentPosition :: IORef WNState -> X Point
currentPosition posRef = do
    root <- asks theRoot
    currentWindow <- gets (W.peek . windowset)
    currentRect <- maybe (Rectangle 0 0 0 0) snd <$> windowRect (fromMaybe root currentWindow)

    wsid <- gets (W.tag . W.workspace . W.current . windowset)
    mp <- M.lookup wsid <$> io (readIORef posRef)

    case mp of
        Just p | p `inside` currentRect -> return p
        _                               -> return (middleOf currentRect)

  where Point px py `inside` Rectangle rx ry rw rh =
            px >= rx && px < rx + fromIntegral rw &&
            py >= rx && py < ry + fromIntegral rh

        middleOf (Rectangle x y w h) =
            Point (x + fromIntegral w `div` 2) (y + fromIntegral h `div` 2)
    -- return $ fromMaybe (Point 0 0) mp

-- TODO: use a smarter algorithm (with memory of last position)
setPosition :: IORef WNState -> Point -> Rectangle -> X ()
setPosition posRef _ (Rectangle x y w h) = do
    wsid <- gets (W.tag . W.workspace . W.current . windowset)
    let position = Point (x + (fromIntegral w `div` 2)) (y + (fromIntegral h `div` 2))
    io $ modifyIORef posRef $ M.insert wsid position

navigableTargets :: Point -> Direction -> X [(Window, Rectangle)]
navigableTargets point dir = navigable dir point <$> windowRects

-- Filters and sorts the windows in terms of what is closest from the Point in
-- the Direction.
navigable :: Direction -> Point -> [(Window, Rectangle)] -> [(Window, Rectangle)]
navigable d pt = sortby d . filter (inr d (fromPoint pt) . snd)

-- Produces a list of normal-state windows, on any screen. Rectangles are
-- adjusted based on screen position relative to the current screen, because I'm
-- bad like that.
-- TODO: only the visible windows
-- TODO: adjust rectangles based on screen position? (perhaps this is already handled)
windowRects :: X [(Window, Rectangle)]
windowRects = do
    wins <- gets (visibleWindows . windowset)
    catMaybes <$> mapM windowRect wins
  where visibleWindows wset = concatMap (W.integrate' . W.stack . W.workspace)
                                        (W.current wset : W.visible wset)

windowRect :: Window -> X (Maybe (Window, Rectangle))
windowRect win = withDisplay $ \dpy -> do
    (_, x, y, w, h, _, _) <- io $ getGeometry dpy win
    return $ Just $ (win, Rectangle x y w h)
    `catchX` return Nothing

fromPoint :: Point -> FPoint
fromPoint p = P (fromIntegral $ pt_x p) (fromIntegral $ pt_y p)

-- Stolen from droundy's implementation of WindowNavigation.
-- TODO: refactor, perhaps

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
