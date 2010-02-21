-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ScreenCorners
-- Copyright   :  (c) 2009 Nils Schweinsberg
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Run @X ()@ actions by touching the edge of your screen the your mouse.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.ScreenCorners
    (
    -- * Usage
    -- $usage
    -- * Event hook
      screenCornerEventHook
    , ScreenCorner (..)

    -- * X11 input methods
    , defaultEventInput
    , adjustEventInput
    ) where

import Data.Monoid
import Foreign.C.Types

import XMonad
import XMonad.Actions.UpdateFocus (adjustEventInput)

data ScreenCorner = SCUpperLeft
                  | SCUpperRight
                  | SCLowerLeft
                  | SCLowerRight

inCorner :: ScreenCorner -> X () -> Display -> CInt -> CInt -> X ()
inCorner corner xF dpy ix iy = do

    let
        screen = defaultScreen dpy
        xMax   = displayWidth  dpy screen - 1
        yMax   = displayHeight dpy screen - 1
        pos    = case (ix,iy, corner) of
                      (0,0, SCUpperLeft)                           -> Just ()
                      (x,0, SCUpperRight) | x == xMax              -> Just ()
                      (0,y, SCLowerLeft)  | y == yMax              -> Just ()
                      (x,y, SCLowerRight) | x == xMax && y == yMax -> Just ()
                      _                                            -> Nothing

    case pos of
         Just _ -> do
             -- Ignore any MotionEvents
             defaultEventInput
             -- Run our X ()
             xF
             -- Handle MotionEvents again
             adjustEventInput

         _ -> return ()

-- | The event hook manager for @ScreenCorners@.
screenCornerEventHook :: Event -> [(ScreenCorner, X ())] -> X All
screenCornerEventHook MotionEvent { ev_event_display = dpy, ev_x = ix, ev_y = iy } lis = do

    mapM_ (\(c,x) -> inCorner c x dpy ix iy) lis
    return $ All True

screenCornerEventHook _ _ = return $ All True


-- | Use the default input methods
defaultEventInput :: X ()
defaultEventInput = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    io $ selectInput dpy rootw $  substructureRedirectMask .|. substructureNotifyMask
                              .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
                              .|. buttonPressMask


-- $usage
--
-- This extension adds KDE-like screen corners to XMonad. By moving your cursor
-- into one of your screen corners you can trigger an @X ()@ action, for
-- example "XMonad.Actions.GridSelect".gotoSelected or
-- "XMonad.Actions.CycleWS".nextWS etc.
--
-- To use it, import it on top of your @xmonad.hs@:
--
-- > import XMonad.Hooks.ScreenCorners
--
-- Then add @adjustEventInput@ to your startup hook:
--
-- > myStartupHook = do
-- >     ...
-- >     adjustEventInput
--
-- And put your custom ScreenCorners to your event hook:
--
-- > myEventHook e = do
-- >     ...
-- >     screenCornerEventHook e [ (SCUpperRight, goToSelected defaultGSConfig { gs_cellwidth = 200 })
-- >                             , (SCLowerRight, nextWS)
-- >                             , (SCLowerLeft,  prevWS)
-- >                             ]
