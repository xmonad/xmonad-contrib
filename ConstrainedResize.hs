-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ConstrainedResize
-- Copyright   :  (c) Dougal Stanton
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  <dougal@dougalstanton.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Lets you constrain the aspect ratio of a floating
-- window by holding shift while you resize.
--
-- Useful for making a nice circular XClock window.
--
-----------------------------------------------------------------------------

module XMonadContrib.ConstrainedResize (
	-- * Usage
	-- $usage
	XMonadContrib.ConstrainedResize.mouseResizeWindow
) where

import XMonad
import Operations
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- Put something like this in your Config.hs file:
--
-- > import qualified XMonadContrib.ConstrainedResize as Sqr
-- > mouseBindings = M.fromList
-- >     [ ...
-- >     , ((modMask, button3),               (\w -> focus w >> Sqr.mouseResizeWindow w False))
-- >     , ((modMask .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True )) ]

-- %import qualified XMonadContrib.ConstrainedResize as Sqr
-- %mousebind , ((modMask, button3), (\\w -> focus w >> Sqr.mouseResizeWindow w False))

-- | Resize (floating) window with optional aspect ratio constraints.
mouseResizeWindow :: Window -> Bool -> X ()
mouseResizeWindow w c = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa)) (fromIntegral (wa_height wa))
    mouseDrag (\ex ey -> do
                 let x = ex - fromIntegral (wa_x wa)
                     y = ey - fromIntegral (wa_y wa)
                     sz = if c then (max x y, max x y) else (x,y)
                 io $ resizeWindow d w `uncurry`
                    applySizeHints sh sz)
              (float w)
