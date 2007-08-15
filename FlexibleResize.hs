-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.FlexibleResize
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Lets you resize floating windows from any corner.
--
-----------------------------------------------------------------------------

module XMonadContrib.FlexibleResize (
	-- * Usage
	-- $usage
	XMonadContrib.FlexibleResize.mouseResizeWindow
) where

import XMonad
import Operations
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign.C.Types

-- $usage
-- Put something like this in your Config.hs file:
--
-- > import qualified XMonadContrib.FlexibleResize as Flex
-- > mouseBindings = M.fromList
-- >     [ ...
-- >     , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    (_, _, _, _, _, ix, iy, _) <- io $ queryPointer d w
    let
        [pos_x, pos_y, width, height] = map (fromIntegral . ($ wa)) [wa_x, wa_y, wa_width, wa_height]
        west  = firstHalf ix width
        north = firstHalf iy height
        (cx, fx, gx) = mkSel west  width  pos_x
        (cy, fy, gy) = mkSel north height pos_y
    io $ warpPointer d none w 0 0 0 0 cx cy
    mouseDrag (\ex ey -> do
                 wa' <- io $ getWindowAttributes d w
                 let [px, py] = map (fromIntegral . ($ wa')) [wa_x, wa_y]
                 io $ moveResizeWindow d w (fx px (fromIntegral ex))
                                           (fy py (fromIntegral ey))
                            `uncurry` applySizeHints sh (gx $ fromIntegral ex, gy $ fromIntegral ey))
              (float w)
    where
    firstHalf :: CInt -> Position -> Bool
    firstHalf a b = fromIntegral a * 2 <= b
    cfst = curry fst
    csnd = curry snd
    mkSel :: Bool -> Position -> Position -> (Position, a -> a -> a, CInt -> Dimension)
    mkSel b k p =
        if b
            then (0, csnd, fromIntegral . max 1 . ((k + p) -) . fromIntegral)
            else (k, cfst, fromIntegral . max 1 . subtract p  . fromIntegral)
