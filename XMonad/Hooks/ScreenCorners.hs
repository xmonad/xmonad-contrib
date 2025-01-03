{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  XMonad.Hooks.ScreenCorners
-- Description :  Run X () actions by touching the edge of your screen with your mouse.
-- Copyright   :  (c) 2009-2025 Nils Schweinsberg, 2015 Evgeny Kurnevsky, 2024 Yuanle Song,
--                2025 Pascal Jaeger
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@nils.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Run @X ()@ actions by touching the edge of your screen with your mouse.
module XMonad.Hooks.ScreenCorners
  ( -- * Usage
    -- $usage

    -- * Adding screen corners
    ScreenCorner (..),
    addScreenCorner,
    addScreenCorners,
    addMonitorCorner,
    addMonitorCorners,

    -- * Event hook
    screenCornerEventHook,

    -- * Layout hook
    screenCornerLayoutHook,
  )
where

import qualified Data.Map as M
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleState as XS
import Graphics.X11.Xinerama (xineramaQueryScreens, XineramaScreenInfo(..))

data ScreenCorner
  = SCUpperLeft
  | SCUpperRight
  | SCLowerLeft
  | SCLowerRight
  | SCTop
  | SCBottom
  | SCLeft
  | SCRight
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- ExtensibleState modifications
--------------------------------------------------------------------------------

newtype ScreenCornerState = ScreenCornerState (M.Map (ScreenCorner, Int) (Window, X ()))

instance ExtensionClass ScreenCornerState where
  initialValue = ScreenCornerState M.empty

-- | Add one single @X ()@ action to a screen corner
{-# DEPRECATED addScreenCorner "addScreenCorner works only in a single monitor setup. Use addMonitorCorner instead." #-}
addScreenCorner :: ScreenCorner -> X () -> X ()
addScreenCorner corner xF = do
  ScreenCornerState m <- XS.get
  let key = (corner, -1) -- Use -1 to indicate a non-monitor-specific corner
  (win, xFunc) <- case M.lookup key m of
    Just (w, xF') -> return (w, xF' >> xF) -- Chain X actions
    Nothing -> (,xF) <$> createWindowAt corner
  XS.modify $ \(ScreenCornerState m') -> ScreenCornerState $ M.insert key (win, xFunc) m'

-- | Add a list of @(ScreenCorner, X ())@ tuples
{-# DEPRECATED addScreenCorners "addScreenCorners works only in a single monitor setup. Use addMonitorCorners instead." #-}
addScreenCorners :: [(ScreenCorner, X ())] -> X ()
addScreenCorners = mapM_ (uncurry addScreenCorner)

-- | Add one single @X ()@ action to a screen corner on a specific monitor
addMonitorCorner :: ScreenCorner -> Int -> Dimension -> X () -> X ()
addMonitorCorner corner monitorNumber hotZoneSize xF = do
  ScreenCornerState m <- XS.get
  let key = (corner, monitorNumber)
  (win, xFunc) <- case M.lookup key m of
    Just (w, xF') -> return (w, xF' >> xF) -- Chain X actions
    Nothing -> (,xF) <$> createWindowAtMonitor corner monitorNumber hotZoneSize
  XS.modify $ \(ScreenCornerState m') -> ScreenCornerState $ M.insert key (win, xFunc) m'

-- | Add a list of @(ScreenCorner, Int, Dimension, X ())@ tuples
addMonitorCorners :: [(ScreenCorner, Int, Dimension, X ())] -> X ()
addMonitorCorners = mapM_ (\(corner, monitor, hotZoneSize, xF) -> addMonitorCorner corner monitor hotZoneSize xF)

--------------------------------------------------------------------------------
-- Xlib functions
--------------------------------------------------------------------------------

-- "Translate" a ScreenCorner to real (x,y) Positions with proper width and height.
createWindowAt :: ScreenCorner -> X Window
createWindowAt SCUpperLeft = createWindowAt' 0 0 1 1
createWindowAt SCUpperRight = withDisplay $ \dpy ->
  let w = displayWidth dpy (defaultScreen dpy) - 1
   in createWindowAt' (fi w) 0 1 1
createWindowAt SCLowerLeft = withDisplay $ \dpy ->
  let h = displayHeight dpy (defaultScreen dpy) - 1
   in createWindowAt' 0 (fi h) 1 1
createWindowAt SCLowerRight = withDisplay $ \dpy ->
  let w = displayWidth dpy (defaultScreen dpy) - 1
      h = displayHeight dpy (defaultScreen dpy) - 1
   in createWindowAt' (fi w) (fi h) 1 1
createWindowAt SCTop = withDisplay $ \dpy ->
  let w = displayWidth dpy (defaultScreen dpy) - 1
      -- leave some gap so corner and edge can work nicely when they overlap
      threshold = 150
   in createWindowAt' threshold 0 (fi $ fi w - threshold * 2) 1
createWindowAt SCBottom = withDisplay $ \dpy ->
  let w = displayWidth dpy (defaultScreen dpy) - 1
      h = displayHeight dpy (defaultScreen dpy) - 1
      threshold = 150
   in createWindowAt' threshold (fi h) (fi $ fi w - threshold * 2) 1
createWindowAt SCLeft = withDisplay $ \dpy ->
  let h = displayHeight dpy (defaultScreen dpy) - 1
      threshold = 150
   in createWindowAt' 0 threshold 1 (fi $ fi h - threshold * 2)
createWindowAt SCRight = withDisplay $ \dpy ->
  let w = displayWidth dpy (defaultScreen dpy) - 1
      h = displayHeight dpy (defaultScreen dpy) - 1
      threshold = 150
   in createWindowAt' (fi w) threshold 1 (fi $ fi h - threshold * 2)

-- "Translate" a ScreenCorner to real (x,y) Positions on a specific monitor
createWindowAtMonitor :: ScreenCorner -> Int -> Dimension -> X Window
createWindowAtMonitor corner monitorNumber hotZoneSize = withDisplay $ \dpy -> do
  screens <- io $ xineramaQueryScreens dpy
  case screens of
    Just scrs | monitorNumber < length scrs -> do
      let XineramaScreenInfo _ x y w h = scrs !! monitorNumber
          hotZoneSize' = fromIntegral hotZoneSize :: Int
          x' = fromIntegral x :: Int
          y' = fromIntegral y :: Int
          w' = fromIntegral w :: Int
          h' = fromIntegral h :: Int
          (xPos, yPos, width, height) = case corner of
            SCUpperLeft -> (x', y', hotZoneSize', hotZoneSize')
            SCUpperRight -> (x' + w' - hotZoneSize', y', hotZoneSize', hotZoneSize')
            SCLowerLeft -> (x', y' + h' - hotZoneSize', hotZoneSize', hotZoneSize')
            SCLowerRight -> (x' + w' - hotZoneSize', y' + h' - hotZoneSize', hotZoneSize', hotZoneSize')
            SCTop -> (x' + 150, y', w' - 300, hotZoneSize')
            SCBottom -> (x' + 150, y' + h' - hotZoneSize', w' - 300, hotZoneSize')
            SCLeft -> (x', y' + 150, hotZoneSize', h' - 300)
            SCRight -> (x' + w' - hotZoneSize', y' + 150, hotZoneSize', h' - 300)
      createWindowAt' (fi xPos) (fi yPos) (fi width) (fi height)
    _ -> error $ "Invalid monitor number or no screens available for monitorNumber=" ++ show monitorNumber

-- Create a new X window at a (x,y) Position, with given width and height.
createWindowAt' :: Position -> Position -> Dimension -> Dimension -> X Window
createWindowAt' x y width height = withDisplay $ \dpy -> io $ do
  rootw <- rootWindow dpy (defaultScreen dpy)

  let visual = defaultVisualOfScreen $ defaultScreenOfDisplay dpy
      attrmask = cWOverrideRedirect

  w <- allocaSetWindowAttributes $ \attributes -> do

    set_override_redirect attributes True
    createWindow dpy        -- display
                 rootw      -- parent window
                 x          -- x
                 y          -- y
                 width      -- width
                 height     -- height
                 0          -- border width
                 0          -- depth
                 inputOnly  -- class
                 visual     -- visual
                 attrmask   -- valuemask
                 attributes -- attributes

  -- we only need mouse entry events  selectInput dpy w enterWindowMask
  selectInput dpy w enterWindowMask
  mapWindow dpy w
  sync dpy False
  return w

--------------------------------------------------------------------------------
-- Event hook
--------------------------------------------------------------------------------

-- | Handle screen corner events
screenCornerEventHook :: Event -> X All
screenCornerEventHook CrossingEvent {ev_window = win} = do
  ScreenCornerState m <- XS.get

  case find (\(_, (w, _)) -> w == win) (M.toList m) of
    Just (_, (_, xF)) -> xF
    Nothing -> return ()

  return (All True)
screenCornerEventHook _ = return (All True)

--------------------------------------------------------------------------------
-- Layout hook
--------------------------------------------------------------------------------

data ScreenCornerLayout a = ScreenCornerLayout
  deriving (Read, Show)

instance LayoutModifier ScreenCornerLayout a where
  hook ScreenCornerLayout = withDisplay $ \dpy -> do
    ScreenCornerState m <- XS.get
    io $ mapM_ (raiseWindow dpy . fst . snd) $ M.toList m
  unhook = hook

screenCornerLayoutHook :: l a -> ModifiedLayout ScreenCornerLayout l a
screenCornerLayoutHook = ModifiedLayout ScreenCornerLayout

--------------------------------------------------------------------------------

-- $usage
--
-- This extension adds KDE-like screen corners and GNOME Hot Edge like
-- features to XMonad. By moving your cursor into one of your screen corners
-- or edges, you can trigger an @X ()@ action, for example
-- @"XMonad.Actions.GridSelect".goToSelected@ or
-- @"XMonad.Actions.CycleWS".nextWS@ etc.
--
-- To use it, import it on top of your @xmonad.hs@:
--
-- > import XMonad.Hooks.ScreenCorners
--
-- Then add your screen corners in our startup hook:
--
-- > myStartupHook = do
-- >     ...
-- >     addMonitorCorner SCUpperLeft 0 20 (spawn "firefox")
-- >     addMonitorCorner SCBottom 0 20 (spawn "firefox")
-- >     addMonitorCorners [ (SCUpperRight, 1, 20, spawn "xterm")
-- >                       , (SCLowerRight, 2, 20, nextWS)
-- >                       ]
-- >     ...
--
-- Where 0-2 are the monitors and 20 is the size of the hot corner.
--
--
-- Then add layout hook:
--
-- > myLayout = screenCornerLayoutHook $ tiled ||| Mirror tiled ||| Full where
-- >     tiled   = Tall nmaster delta ratio
-- >     nmaster = 1
-- >     ratio   = 1 / 2
-- >     delta   = 3 / 100
--
-- And finally wait for screen corner events in your event hook:
--
-- > myEventHook e = do
-- >     ...
-- >     screenCornerEventHook e
