{-# LANGUAGE TupleSections #-} -- I didn't want this, it's hlint's "suggestion" and it's apparently non-negotiable
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowNavigation
-- Description :  Experimental rewrite of "XMonad.Layout.WindowNavigation".
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>,
--                          Devin Mullins <me@twifkak.com>
-- Maintainer  :  Devin Mullins <me@twifkak.com>,
--                Platon Pronko <platon7pronko@gmail.com>
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
                                       goPure, swapPure,
                                       Direction2D(..), WNState,
                                       ) where

import XMonad hiding (state)
import XMonad.Prelude (catMaybes, fromMaybe, sortOn)
import XMonad.Util.Types (Direction2D(..))
import qualified XMonad.StackSet as W

import Control.Arrow (second)
import Data.IORef
import Data.Map (Map())
import Data.List (partition, find)
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
    stateRef <- newIORef M.empty
    return conf { keys = \cnf -> M.fromList (map (second (fromWNAction stateRef)) wnKeys)
                                 `M.union` keys conf cnf,
                  logHook = logHook conf >> trackMovement stateRef }
  where fromWNAction stateRef (WNGo dir)   = go   stateRef dir
        fromWNAction stateRef (WNSwap dir) = swap stateRef dir

data WNAction = WNGo Direction2D | WNSwap Direction2D

type WNState = Map WorkspaceId Point

-- | Focus window in the given direction.
go :: IORef WNState -> Direction2D -> X ()
go stateRef dir = runPureAction stateRef (goPure dir)

-- | Swap current window with the window in the given direction.
-- Note: doesn't work with floating windows (don't think it makes much sense to swap floating windows).
swap :: IORef WNState -> Direction2D -> X ()
swap stateRef dir = runPureAction stateRef (swapPure dir)

type WindowRectFn x = (Window -> x (Maybe Rectangle))
-- | (state, oldWindowSet, mappedWindows, windowRect)
type WNInput x = (WNState, WindowSet, S.Set Window, WindowRectFn x)
type WNOutput = (WNState, WindowSet)

-- | Run the pure action inside X monad.
runPureAction :: IORef WNState -> (WNInput X -> X WNOutput) -> X ()
runPureAction stateRef action = do
  oldState <- io (readIORef stateRef)
  oldWindowSet <- gets windowset
  mappedWindows <- gets mapped
  (newState, newWindowSet) <- action (oldState, oldWindowSet, mappedWindows, windowRectX)
  windows (const newWindowSet)
  io $ writeIORef stateRef newState

-- | Version of `go` not dependent on X monad (needed for testing).
goPure :: Monad x => Direction2D -> WNInput x -> x WNOutput
goPure dir input@(oldState, oldWindowSet, mappedWindows, _) =
  if length (filter (`S.member` mappedWindows) $ W.integrate' $ W.stack $ W.workspace $ W.current oldWindowSet) == 1
  then
    -- Handle the special case of Full layout, when there's only one mapped window on a screen.
    return ( oldState
           , case dir of
               U -> W.focusUp oldWindowSet
               L -> W.focusDown oldWindowSet
               D -> W.focusDown oldWindowSet
               R -> W.focusUp oldWindowSet
           )
  else
    withTargetWindow W.focusWindow dir input

-- | Version of `swap` not dependent on X monad (needed for testing).
swapPure :: Monad x => Direction2D -> WNInput x -> x WNOutput
swapPure = withTargetWindow swapWithFocused
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

-- | Select a target window in the given direction and modify the WindowSet.
-- 1. Get current position, verifying it matches the current window (exit if no focused window).
-- 2. Get the target window.
-- 3. Execute an action on the target window and windowset.
-- 4. Set the new position.
withTargetWindow :: Monad x => (Window -> WindowSet -> WindowSet) -> Direction2D -> WNInput x -> x WNOutput
withTargetWindow adj dir input@(oldState, oldWindowSet, _, _) = do
  whenJust' (getCurrentWindow input) (oldState, oldWindowSet) $ \(win, winRect, pos) -> do
    targetMaybe <- find ((/= win) . fst) <$> navigableTargets input dir winRect pos
    whenJust' (pure targetMaybe) (oldState, oldWindowSet) $ \(targetWin, newPos) ->
      let newWindowSet = adj targetWin oldWindowSet
      in return (modifyState newWindowSet newPos oldState, newWindowSet)

-- | Update position on outside changes in windows.
trackMovement :: IORef WNState -> X ()
trackMovement stateRef = do
  oldState <- io (readIORef stateRef)
  oldWindowSet <- gets windowset
  mappedWindows <- gets mapped
  whenJust' (getCurrentWindow (oldState, oldWindowSet, mappedWindows, windowRectX)) () $ \(_, _, pos) -> do
      io $ writeIORef stateRef $ modifyState oldWindowSet pos oldState

-- | Get focused window and current position.
getCurrentWindow :: Monad x => WNInput x -> x (Maybe (Window, Rectangle, Point))
getCurrentWindow input@(_, oldWindowSet, _, _) =
  whenJust' (pure $ W.peek oldWindowSet) Nothing $ \window -> do
    (pos, rect) <- currentPosition input
    return $ Just (window, rect, pos)

-- | Gets the current position from the state passed in, or if nothing
-- (say, from a restart), derives the current position from the current window.
-- Also, verifies that the position is congruent with the current window
-- (say, if you moved focus using mouse or something).
-- Returns the window rectangle for convenience, since we'll need it later anyway.
currentPosition :: Monad x => WNInput x -> x (Point, Rectangle)
currentPosition (state, oldWindowSet, _, windowRect) = do
  currentRect <- fromMaybe (Rectangle 0 0 0 0) <$> maybe (pure Nothing) windowRect (W.peek oldWindowSet)
  let posMaybe = M.lookup (W.currentTag oldWindowSet) state
      middleOf (Rectangle x y w h) = Point (midPoint x w) (midPoint y h)
  return $ case posMaybe of
    Nothing -> (middleOf currentRect, currentRect)
    Just pos -> (centerPosition currentRect pos, currentRect)

-- | Inserts new position into the state.
modifyState :: WindowSet -> Point -> WNState -> WNState
modifyState oldWindowSet =
  M.insert (W.currentTag oldWindowSet)

-- | "Jumps" the current position into the middle of target rectangle.
-- (keeps the position as-is if it is already inside the target rectangle)
centerPosition :: Rectangle -> Point -> Point
centerPosition r@(Rectangle rx ry rw rh) pos@(Point x y) = do
  if pointWithin x y r
  then pos
  else Point (midPoint rx rw) (midPoint ry rh)

midPoint :: Position -> Dimension -> Position
midPoint pos dim = pos + fromIntegral dim `div` 2

-- | Make a list of target windows we can navigate to,
-- sorted by desirability of navigation.
navigableTargets :: Monad x => WNInput x -> Direction2D -> Rectangle -> Point -> x [(Window, Point)]
navigableTargets input@(_, oldWindowSet, _, _) dir currentRect currentPos = do
  allScreensWindowsAndRectangles <- mapSnd (rectTransform dir) <$> windowRects input
  let
    screenWindows = S.fromList $ W.integrate' $ W.stack $ W.workspace $ W.current oldWindowSet
    (thisScreenWindowsAndRectangles, otherScreensWindowsAndRectangles) = partition (\(w, _) -> S.member w screenWindows) allScreensWindowsAndRectangles

    pos = pointTransform dir currentPos
    wr = rectTransform dir currentRect

    rectInside r = (rect_p1 r >= rect_p1 wr && rect_p1 r < rect_p2 wr && rect_p2 r > rect_p1 wr && rect_p2 r <= rect_p2 wr) &&
                   ((rect_o1 r >= rect_o1 wr && rect_o1 r < rect_o2 wr && rect_o2 r > rect_o1 wr && rect_o2 r <= rect_o2 wr) ||
                    (rect_o1 r <= rect_o1 wr && rect_o2 r >= rect_o2 wr)) -- include windows that fully overlaps current on the orthogonal axis
    sortByP2 = sortOn (rect_p2 . snd)
    posBeforeEdge r = point_p pos < rect_p2 r

    rectOverlapsEdge r = rect_p1 r <= rect_p2 wr && rect_p2 r > rect_p2 wr &&
                         rect_o1 r < rect_o2 wr && rect_o2 r > rect_o1 wr
    rectOverlapsOneEdge r = rectOverlapsEdge r && rect_p1 r > rect_p1 wr
    rectOverlapsBothEdges r = rectOverlapsEdge r &&
                              rect_o1 r > rect_o1 wr && rect_o2 r < rect_o2 wr && point_o pos >= rect_o1 r && point_o pos < rect_o2 r
    distanceToRectEdge r = max (max 0 (rect_o1 r - point_o pos)) (max 0 (point_o pos + 1 - rect_o2 r))
    distanceToRectCenter r =
      let distance = (rect_o1 r + rect_o2 r) `div` 2 - point_o pos
      in if distance <= 0
         then distance + 1
         else distance
    sortByPosDistance = sortOn ((\r -> (rect_p1 r, distanceToRectEdge r, distanceToRectCenter r)) . snd)

    rectOutside r = rect_p1 r < rect_p1 wr && rect_p2 r > rect_p2 wr &&
                    rect_o1 r < rect_o1 wr && rect_o2 r > rect_o2 wr
    sortByLength = sortOn (rect_psize . snd)

    rectAfterEdge r = rect_p1 r > rect_p2 wr

    -- Modified from David Roundy and Devin Mullins original implementation of WindowNavigation:
    inr r = point_p pos < rect_p2 r && point_o pos >= rect_o1 r && point_o pos < rect_o2 r

    clamp v v1 v2 | v < v1 = v1
                  | v >= v2 = v2 - 1
                  | otherwise = v
    dragPos r = DirPoint (max (point_p pos) (rect_p1 r)) (clamp (point_o pos) (rect_o1 r) (rect_o2 r))

  return $ mapSnd (inversePointTransform dir) $ concat
    [
      -- First, navigate to windows that are fully inside current window
      -- and have higher coordinate bigger than current position.
      -- ┌──────────────────┐
      -- │   current        │  (all examples assume direction=R)
      -- │    ┌──────────┐  │
      -- │  ──┼─► inside │  │
      -- │    └──────────┘  │
      -- └──────────────────┘
      -- Also include windows fully overlapping current on the orthogonal axis:
      --             ┌──────────────┐
      --             │ overlapping  │
      -- ┌───────────┤              ├────┐
      -- │ current ──┼─►            │    │
      -- └───────────┤              ├────┘
      --             └──────────────┘
      mapSnd dragPos $ sortByP2 $ filterSnd posBeforeEdge $ filterSnd rectInside thisScreenWindowsAndRectangles

      -- Then navigate to windows that touch or overlap the edge of current window in the chosen direction.
      -- ┌──────────────┬─────────────┐   ┌───────────┐                   ┌─────────────┐
      -- │ current      │ adjacent    │   │ current   │                   │ current     │
      -- │            ──┼─►           │   │       ┌───┴───────────────┐   │         ┌───┴─────────────┐
      -- │              │             │   │     ──┼─► │   overlapping │   │       ──┼─►               │
      -- │              ├─────────────┘   │       └───┬───────────────┘   └─────────┤     overlapping │
      -- │              │                 │           │                             │                 │
      -- └──────────────┘                 └───────────┘                             └─────────────────┘
    , mapSnd dragPos $ sortByPosDistance $ filterSnd rectOverlapsOneEdge thisScreenWindowsAndRectangles

      -- Windows fully overlapping current window "in the middle" on the parallel axis are also included,
      -- if position is inside them:
      --     ┌───────────┐
      --     │  current  │
      -- ┌───┤-----------├────────────────┐
      -- │   │     *   ──┼─►  overlapping │
      -- └───┤-----------├────────────────┘
      --     └───────────┘
    , mapSnd (\_ -> DirPoint (rect_p2 wr) (point_o pos)) $ sortByPosDistance $ filterSnd rectOverlapsBothEdges thisScreenWindowsAndRectangles

      -- Then navigate to windows that fully encompass the current window.
      -- ┌─────────────────────┐
      -- │    outer            │
      -- │  ┌─────────────┐    │
      -- │  │  current  ──┼─►  │
      -- │  └─────────────┘    │
      -- └─────────────────────┘
    , mapSnd (\_ -> DirPoint (rect_p2 wr) (point_o pos)) $ sortByLength $ filterSnd rectOutside thisScreenWindowsAndRectangles

      -- Then navigate to windows that are fully after current window in the chosen direction.
      -- ┌──────────────┐
      -- │ current      │  ┌────────────────┐
      -- │              │  │                │
      -- │            ──┼──┼─► not adjacent │
      -- │              │  │                │
      -- │              │  └────────────────┘
      -- └──────────────┘
    , mapSnd dragPos $ sortByPosDistance $ filterSnd rectAfterEdge thisScreenWindowsAndRectangles

      -- Cast a ray from the current position, jump to the first window (on another screen) that intersects this ray.
    , mapSnd dragPos $ sortByPosDistance $ filterSnd inr otherScreensWindowsAndRectangles

      -- If everything else fails, then navigate to the window that is fully inside current window,
      -- but is before the current position.
      -- This can happen when we are at the last window on a screen, and attempt to navigate even further.
      -- In this case it seems okay to jump to the remaining inner windows, since we don't have any other choice anyway,
      -- and user is probably not so fully aware of the precise position anyway.
    , mapSnd (\r -> DirPoint (rect_p2 r - 1) (clamp (point_o pos) (rect_o1 r) (rect_o2 r))) $
      sortByP2 $ filterSnd (not . posBeforeEdge) $ filterSnd rectInside thisScreenWindowsAndRectangles
    ]

-- Structs for direction-independent space - equivalent to rotating points and rectangles such that
-- navigation direction points to the right.
-- Allows us to abstract over direction in the navigation functions.
data DirPoint = DirPoint
  { point_p :: Position -- coordinate parallel to the direction
  , point_o :: Position -- coordinate orthogonal to the direction
  }
data DirRectangle = DirRectangle
  { rect_p1 :: Position -- lower rectangle coordinate parallel to the direction
  , rect_p2 :: Position -- higher rectangle coordinate parallel to the direction
  , rect_o1 :: Position -- lower rectangle coordinate orthogonal to the direction
  , rect_o2 :: Position -- higher rectangle coordinate orthogonal to the direction
  }
{- HLINT ignore "Use camelCase" -}
rect_psize :: DirRectangle -> Dimension
rect_psize r = fromIntegral (rect_p2 r - rect_p1 r)

-- | Transform a point from screen space into direction-independent space.
pointTransform :: Direction2D -> Point -> DirPoint
pointTransform dir (Point x y) = case dir of
  U -> DirPoint (negate y - 1) x
  L -> DirPoint (negate x - 1) (negate y - 1)
  D -> DirPoint y (negate x - 1)
  R -> DirPoint x y

-- | Transform a point from direction-independent space back into screen space.
inversePointTransform :: Direction2D -> DirPoint -> Point
inversePointTransform dir p = case dir of
  U -> Point (point_o p) (negate $ point_p p + 1)
  L -> Point (negate $ point_p p + 1) (negate $ point_o p + 1)
  D -> Point (negate $ point_o p + 1) (point_p p)
  R -> Point (point_p p) (point_o p)

-- | Transform a rectangle from screen space into direction-independent space.
rectTransform :: Direction2D -> Rectangle -> DirRectangle
rectTransform dir (Rectangle x y w h) = case dir of
  U -> DirRectangle (negate $ y + fromIntegral h) (negate y) x (x + fromIntegral w)
  L -> DirRectangle (negate $ x + fromIntegral w) (negate x) (negate $ y + fromIntegral h) (negate y)
  D -> DirRectangle y (y + fromIntegral h) (negate $ x + fromIntegral w) (negate x)
  R -> DirRectangle x (x + fromIntegral w) y (y + fromIntegral h)

-- | Produces a list of normal-state windows on all screens, excluding currently focused window.
windowRects :: Monad x => WNInput x -> x [(Window, Rectangle)]
windowRects (_, oldWindowSet, mappedWindows, windowRect) =
  let
    allWindows = filter (\w -> w `notElem` W.peek oldWindowSet) $ S.toList mappedWindows
    windowRect2 w = fmap (w,) <$> windowRect w
  in catMaybes <$> mapM windowRect2 allWindows

windowRectX :: Window -> X (Maybe Rectangle)
windowRectX win = withDisplay $ \dpy -> do
    (_, x, y, w, h, bw, _) <- io $ getGeometry dpy win
    return $ Just $ Rectangle x y (w + 2 * bw) (h + 2 * bw)
    `catchX` return Nothing

-- Maybe below functions can be replaced with some standard helper functions?

-- | Execute a monadic action on the contents if Just, otherwise wrap default value and return it.
whenJust' :: Monad x => x (Maybe a) -> b -> (a -> x b) -> x b
whenJust' monadMaybeValue deflt f = do
  maybeValue <- monadMaybeValue
  case maybeValue of
    Nothing -> return deflt
    Just value -> f value

-- | Filter a list of tuples on the second tuple member.
filterSnd :: (b -> Bool) -> [(a, b)] -> [(a, b)]
filterSnd f = filter (f . snd)

-- | Map a second tuple member in a list of tuples.
mapSnd :: (b -> b') -> [(a, b)] -> [(a, b')]
mapSnd f = map (second f)
