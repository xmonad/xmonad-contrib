-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowNavigation
-- Description :  Experimental rewrite of "XMonad.Layout.WindowNavigation".
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>,
--                          Devin Mullins <me@twifkak.com>
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- License     :  BSD3-style (see LICENSE)
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a rewrite of "XMonad.Layout.WindowNavigation".  WindowNavigation
-- lets you assign keys to move up\/down\/left\/right, based on actual cartesian
-- window coordinates, rather than just going j\/k on the stack.
--
-- This module is experimental. You'll have better luck with the original.
--
-- This module differs from the other in a few ways:
--
--   (1) You can go up\/down\/left\/right across multiple screens.
--
--   (2) It doesn't provide little border colors for your neighboring windows.
--
--   (3) It doesn't provide the \'Move\' action, which seems to be related to
--      the XMonad.Layout.Combo extension.
--
--   (4) It tries to be slightly smarter about tracking your current position.
--
--   (5) Configuration is different.
--
-----------------------------------------------------------------------------

module XMonad.Actions.WindowNavigation (
                                       -- * Usage
                                       -- $usage
                                       withWindowNavigation,
                                       withWindowNavigationKeys,
                                       WNAction(..),
                                       go, swap,
                                       Direction2D(..), WNState,
                                       ) where

import XMonad
import XMonad.Prelude (catMaybes, fromMaybe, listToMaybe, sortOn)
import XMonad.Util.Types (Direction2D(..))
import qualified XMonad.StackSet as W

import Control.Arrow (second)
import Data.IORef
import Data.Map (Map())
import qualified Data.Map as M
import qualified Data.Set as S

-- $usage
--
-- To use it, you're going to apply the 'withWindowNavigation' function.
-- 'withWindowNavigation' performs some IO operations, so the syntax you'll use
-- is the same as the spawnPipe example in "XMonad.Hooks.DynamicLog".
-- In particular:
--
-- > main = do
-- >     config <- withWindowNavigation (xK_w, xK_a, xK_s, xK_d)
-- >             $ def { ... }
-- >     xmonad config
--
-- Or, for the brave souls:
--
-- > main = xmonad =<< withWindowNavigation (xK_w, xK_a, xK_s, xK_d)
-- >             $ def { ... }
--
-- Here, we pass in the keys for navigation in counter-clockwise order from up.
-- It creates keybindings for @modMask@ to move to window, and @modMask .|. shiftMask@
-- to swap windows.
--
-- If you want more flexibility over your keybindings, you can use
-- 'withWindowNavigationKeys', which takes a list of @keys@-esque entries rather
-- than a tuple of the four directional keys. See the source code of
-- 'withWindowNavigation' for an example.

-- TODO:
--  - monad for WNState?
--  - cleanup (including inr)
--  - more documentation
--  - tests? (esp. for edge cases in currentPosition)
--  - screen 1, 1+2/w 3, M-d, M-w, M-2 (1+2/w 2), M-e, M-a - goes to w 3, should be w 2
--  - solve the 2+3, middle right to bottom left problem
--  - command to iteratively swapUp/swapDown instead of directly swapping with target
--  - manageHook to draw window decos?

withWindowNavigation :: (KeySym, KeySym, KeySym, KeySym) -> XConfig l -> IO (XConfig l)
withWindowNavigation (u,l,d,r) conf@XConfig{modMask=modm} =
    withWindowNavigationKeys [ ((modm              , u), WNGo   U),
                               ((modm              , l), WNGo   L),
                               ((modm              , d), WNGo   D),
                               ((modm              , r), WNGo   R),
                               ((modm .|. shiftMask, u), WNSwap U),
                               ((modm .|. shiftMask, l), WNSwap L),
                               ((modm .|. shiftMask, d), WNSwap D),
                               ((modm .|. shiftMask, r), WNSwap R) ]
                             conf

withWindowNavigationKeys :: [((KeyMask, KeySym), WNAction)] -> XConfig l -> IO (XConfig l)
withWindowNavigationKeys wnKeys conf = do
    posRef <- newIORef M.empty
    return conf { keys = \cnf -> M.fromList (map (second (fromWNAction posRef)) wnKeys)
                                 `M.union` keys conf cnf,
                  logHook = logHook conf >> trackMovement posRef }
  where fromWNAction posRef (WNGo dir)   = go   posRef dir
        fromWNAction posRef (WNSwap dir) = swap posRef dir

data WNAction = WNGo Direction2D | WNSwap Direction2D

type WNState = Map WorkspaceId Point

-- go:
-- 1. get current position, verifying it matches the current window
-- 2. get target windowrect
-- 3. focus window
-- 4. set new position
go :: IORef WNState -> Direction2D -> X ()
go = withTargetWindow W.focusWindow

swap :: IORef WNState -> Direction2D -> X ()
swap = withTargetWindow swapWithFocused
  where swapWithFocused targetWin winSet =
            case W.peek winSet of
                Just currentWin -> W.focusWindow currentWin $
                                   mapWindows (swapWin currentWin targetWin) winSet
                Nothing -> winSet
        mapWindows f = W.mapWorkspace (mapWindows' f)
        mapWindows' f ws@W.Workspace{ W.stack = s } = ws { W.stack = mapWindows'' f <$> s }
        mapWindows'' f (W.Stack focused up down) = W.Stack (f focused) (map f up) (map f down)
        swapWin win1 win2 win
          | win == win1 = win2
          | win == win2 = win1
          | otherwise = win

withTargetWindow :: (Window -> WindowSet -> WindowSet) -> IORef WNState -> Direction2D -> X ()
withTargetWindow adj posRef dir = fromCurrentPoint posRef $ \win pos -> do
    targets <- filter ((/= win) . fst) <$> navigableTargets pos dir
    whenJust (listToMaybe targets) $ \(targetWin, targetRect) -> do
      windows (adj targetWin)
      setPosition posRef pos targetRect

trackMovement :: IORef WNState -> X ()
trackMovement posRef = fromCurrentPoint posRef $ \win pos ->
                           windowRect win >>= flip whenJust (setPosition posRef pos . snd)

fromCurrentPoint :: IORef WNState -> (Window -> Point -> X ()) -> X ()
fromCurrentPoint posRef f = withFocused $ \win ->
                                currentPosition posRef >>= f win

-- Gets the current position from the IORef passed in, or if nothing (say, from
-- a restart), derives the current position from the current window. Also,
-- verifies that the position is congruent with the current window (say, if you
-- used mod-j/k or mouse or something).
currentPosition :: IORef WNState -> X Point
currentPosition posRef = do
    root <- asks theRoot
    currentWindow <- gets (W.peek . windowset)
    currentRect <- maybe (Rectangle 0 0 0 0) snd <$> windowRect (fromMaybe root currentWindow)

    wsid <- gets (W.currentTag . windowset)
    mp <- M.lookup wsid <$> io (readIORef posRef)

    return $ maybe (middleOf currentRect) (`inside` currentRect) mp

  where middleOf (Rectangle x y w h) = Point (midPoint x w) (midPoint y h)

setPosition :: IORef WNState -> Point -> Rectangle -> X ()
setPosition posRef oldPos newRect = do
    wsid <- gets (W.currentTag . windowset)
    io $ modifyIORef posRef $ M.insert wsid (oldPos `inside` newRect)

inside :: Point -> Rectangle -> Point
Point x y `inside` Rectangle rx ry rw rh =
    Point (x `within` (rx, rw)) (y `within` (ry, rh))
  where pos `within` (lower, dim) = if pos >= lower && pos < lower + fromIntegral dim
                                    then pos
                                    else midPoint lower dim

midPoint :: Position -> Dimension -> Position
midPoint pos dim = pos + fromIntegral dim `div` 2

navigableTargets :: Point -> Direction2D -> X [(Window, Rectangle)]
navigableTargets point dir = navigable dir point <$> windowRects

-- Filters and sorts the windows in terms of what is closest from the Point in
-- the Direction2D.
navigable :: Direction2D -> Point -> [(Window, Rectangle)] -> [(Window, Rectangle)]
navigable d pt = sortby d . filter (inr d pt . snd)

-- Produces a list of normal-state windows, on any screen. Rectangles are
-- adjusted based on screen position relative to the current screen, because I'm
-- bad like that.
windowRects :: X [(Window, Rectangle)]
windowRects = fmap catMaybes . mapM windowRect . S.toList =<< gets mapped

windowRect :: Window -> X (Maybe (Window, Rectangle))
windowRect win = withDisplay $ \dpy -> do
    (_, x, y, w, h, bw, _) <- io $ getGeometry dpy win
    return $ Just (win, Rectangle x y (w + 2 * bw) (h + 2 * bw))
    `catchX` return Nothing

-- Modified from droundy's implementation of WindowNavigation:

inr :: Direction2D -> Point -> Rectangle -> Bool
inr D (Point px py) (Rectangle rx ry w h) = px >= rx && px < rx + fromIntegral w &&
                                                        py < ry + fromIntegral h
inr U (Point px py) (Rectangle rx ry w _) = px >= rx && px < rx + fromIntegral w &&
                                            py >  ry
inr R (Point px py) (Rectangle rx ry w h) =             px < rx + fromIntegral w &&
                                            py >= ry && py < ry + fromIntegral h
inr L (Point px py) (Rectangle rx ry _ h) = px >  rx &&
                                            py >= ry && py < ry + fromIntegral h

sortby :: Direction2D -> [(a,Rectangle)] -> [(a,Rectangle)]
sortby D = sortOn (rect_y . snd)
sortby R = sortOn (rect_x . snd)
sortby U = reverse . sortby D
sortby L = reverse . sortby R
