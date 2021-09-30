{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ScreenCorners
-- Description :  Run X () actions by touching the edge of your screen with your mouse.
-- Copyright   :  (c) 2009 Nils Schweinsberg, 2015 Evgeny Kurnevsky
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Run @X ()@ actions by touching the edge of your screen with your mouse.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.ScreenCorners
    (
    -- * Usage
    -- $usage

    -- * Adding screen corners
      ScreenCorner (..)
    , addScreenCorner
    , addScreenCorners

    -- * Event hook
    , screenCornerEventHook

    -- * Layout hook
    , screenCornerLayoutHook
    ) where

import XMonad.Prelude
import XMonad
import XMonad.Layout.LayoutModifier

import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS

data ScreenCorner = SCUpperLeft
                  | SCUpperRight
                  | SCLowerLeft
                  | SCLowerRight
                  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- ExtensibleState modifications
--------------------------------------------------------------------------------

newtype ScreenCornerState = ScreenCornerState (M.Map Window (ScreenCorner, X ()))

instance ExtensionClass ScreenCornerState where
    initialValue = ScreenCornerState M.empty

-- | Add one single @X ()@ action to a screen corner
addScreenCorner :: ScreenCorner -> X () -> X ()
addScreenCorner corner xF = do

    ScreenCornerState m <- XS.get
    (win,xFunc) <- case find (\(_,(sc,_)) -> sc == corner) (M.toList m) of

                        Just (w, (_,xF')) -> return (w, xF' >> xF) -- chain X actions
                        Nothing           -> (, xF) <$> createWindowAt corner

    XS.modify $ \(ScreenCornerState m') -> ScreenCornerState $ M.insert win (corner,xFunc) m'

-- | Add a list of @(ScreenCorner, X ())@ tuples
addScreenCorners :: [ (ScreenCorner, X ()) ] -> X ()
addScreenCorners = mapM_ (uncurry addScreenCorner)


--------------------------------------------------------------------------------
-- Xlib functions
--------------------------------------------------------------------------------

-- "Translate" a ScreenCorner to real (x,y) Positions
createWindowAt :: ScreenCorner -> X Window
createWindowAt SCUpperLeft = createWindowAt' 0 0
createWindowAt SCUpperRight = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy) - 1
    in createWindowAt' (fi w) 0

createWindowAt SCLowerLeft = withDisplay $ \dpy ->
    let h = displayHeight dpy (defaultScreen dpy) - 1
    in createWindowAt' 0 (fi h)

createWindowAt SCLowerRight = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy) - 1
        h = displayHeight dpy (defaultScreen dpy) - 1
    in createWindowAt' (fi w) (fi h)

-- Create a new X window at a (x,y) Position
createWindowAt' :: Position -> Position -> X Window
createWindowAt' x y = withDisplay $ \dpy -> io $ do

    rootw <- rootWindow dpy (defaultScreen dpy)

    let
        visual   = defaultVisualOfScreen $ defaultScreenOfDisplay dpy
        attrmask = cWOverrideRedirect

    w <- allocaSetWindowAttributes $ \attributes -> do

        set_override_redirect attributes True
        createWindow dpy        -- display
                     rootw      -- parent window
                     x          -- x
                     y          -- y
                     1          -- width
                     1          -- height
                     0          -- border width
                     0          -- depth
                     inputOnly  -- class
                     visual     -- visual
                     attrmask   -- valuemask
                     attributes -- attributes

    -- we only need mouse entry events
    selectInput dpy w enterWindowMask
    mapWindow dpy w
    sync dpy False
    return w


--------------------------------------------------------------------------------
-- Event hook
--------------------------------------------------------------------------------

-- | Handle screen corner events
screenCornerEventHook :: Event -> X All
screenCornerEventHook CrossingEvent { ev_window = win } = do

    ScreenCornerState m <- XS.get

    case M.lookup win m of
         Just (_, xF) -> xF
         Nothing      -> return ()

    return (All True)

screenCornerEventHook _ = return (All True)


--------------------------------------------------------------------------------
-- Layout hook
--------------------------------------------------------------------------------

data ScreenCornerLayout a = ScreenCornerLayout
    deriving ( Read, Show )

instance LayoutModifier ScreenCornerLayout a where
    hook ScreenCornerLayout = withDisplay $ \dpy -> do
        ScreenCornerState m <- XS.get
        io $ mapM_ (raiseWindow dpy) $ M.keys m
    unhook = hook

screenCornerLayoutHook :: l a -> ModifiedLayout ScreenCornerLayout l a
screenCornerLayoutHook = ModifiedLayout ScreenCornerLayout


--------------------------------------------------------------------------------
-- $usage
--
-- This extension adds KDE-like screen corners to XMonad. By moving your cursor
-- into one of your screen corners you can trigger an @X ()@ action, for
-- example @"XMonad.Actions.GridSelect".goToSelected@ or
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
-- >     addScreenCorner SCUpperRight (goToSelected def { gs_cellwidth = 200})
-- >     addScreenCorners [ (SCLowerRight, nextWS)
-- >                      , (SCLowerLeft,  prevWS)
-- >                      ]
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
