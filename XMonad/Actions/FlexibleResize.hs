-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.FlexibleResize
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Resize floating windows from any corner.
--
-----------------------------------------------------------------------------

module XMonad.Actions.FlexibleResize (
	-- * Usage
	-- $usage
	XMonad.Actions.FlexibleResize.mouseResizeWindow
) where

import XMonad
import Foreign.C.Types

-- $usage
-- To use, first import this module into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import qualified XMonad.Actions.FlexibleResize as Flex
--
-- Then add an appropriate mouse binding:
--
-- >     , ((modMask x, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

-- | Resize a floating window from whichever corner the mouse is
--   closest to.
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
    mkSel :: Bool -> Position -> Position -> (Position, a -> a -> a, CInt -> Position)
    mkSel b k p =
        if b
            then (0, csnd, ((k + p) -) . fromIntegral)
            else (k, cfst, subtract p  . fromIntegral)
