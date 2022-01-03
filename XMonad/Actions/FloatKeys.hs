-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.FloatKeys
-- Description  :  Move and resize floating windows.
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : stable
-- Portability  : unportable
--
-- Move and resize floating windows.
-----------------------------------------------------------------------------

module XMonad.Actions.FloatKeys (
                -- * Usage
                -- $usage
                keysMoveWindow,
                keysMoveWindowTo,
                keysResizeWindow,
                keysAbsResizeWindow,
                P, G, ChangeDim
                ) where

import XMonad
import XMonad.Prelude (fi)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.FloatKeys
--
-- Then add appropriate key bindings, for example:
--
-- >  , ((modm,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1,1)))
-- >  , ((modm,               xK_s     ), withFocused (keysResizeWindow (10,10) (1,1)))
-- >  , ((modm .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
-- >  , ((modm .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
-- >  , ((modm,               xK_a     ), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | @keysMoveWindow (dx, dy)@ moves the window by @dx@ pixels to the
--   right and @dy@ pixels down.
keysMoveWindow :: ChangeDim -> Window -> X ()
keysMoveWindow (dx,dy) w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    io $ moveWindow d w (fi (fi (wa_x wa) + dx))
                        (fi (fi (wa_y wa) + dy))
    float w

-- | @keysMoveWindowTo (x, y) (gx, gy)@ moves the window relative
--   point @(gx, gy)@ to the point @(x,y)@, where @(gx,gy)@ gives a
--   position relative to the window border, i.e.  @gx = 0@ is the left
--   border, @gx = 1@ is the right border, @gy = 0@ is the top border, and
--   @gy = 1@ the bottom border.
--
--   For example, on a 1024x768 screen:
--
-- > keysMoveWindowTo (512,384) (1%2, 1%2) -- center the window on screen
-- > keysMoveWindowTo (1024,0) (1, 0)      -- put window in the top right corner
keysMoveWindowTo :: P -> G -> Window -> X ()
keysMoveWindowTo (x,y) (gx, gy) w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    io $ moveWindow d w (x - round (gx * fi (wa_width wa)))
                        (y - round (gy * fi (wa_height wa)))
    float w

type G = (Rational, Rational)
type P = (Position, Position)
type ChangeDim = (Int, Int)

-- | @keysResizeWindow (dx, dy) (gx, gy)@ changes the width by @dx@
--   and the height by @dy@, leaving the window-relative point @(gx,
--   gy)@ fixed.
--
--   For example:
--
-- > keysResizeWindow (10, 0) (0, 0)      -- make the window 10 pixels larger to the right
-- > keysResizeWindow (10, 0) (0, 1%2)    -- does the same, unless sizeHints are applied
-- > keysResizeWindow (10, 10) (1%2, 1%2) -- add 5 pixels on each side
-- > keysResizeWindow (-10, -10) (0, 1)   -- shrink the window in direction of the bottom-left corner
keysResizeWindow :: ChangeDim -> G -> Window -> X ()
keysResizeWindow = keysMoveResize keysResizeWindow'

-- | @keysAbsResizeWindow (dx, dy) (ax, ay)@ changes the width by @dx@
--   and the height by @dy@, leaving the screen absolute point @(ax,
--   ay)@ fixed.
--
--   For example:
--
-- > keysAbsResizeWindow (10, 10) (0, 0)   -- enlarge the window; if it is not in the top-left corner it will also be moved down and to the right.
keysAbsResizeWindow :: ChangeDim -> D -> Window -> X ()
keysAbsResizeWindow = keysMoveResize keysAbsResizeWindow'

keysAbsResizeWindow' :: SizeHints -> P -> D -> ChangeDim -> D -> (P,D)
keysAbsResizeWindow' sh (x,y) (w,h) (dx,dy) (ax, ay) = ((round nx, round ny), (nw, nh))
    where
        -- The width and height of a window are positive and thus
        -- converting to 'Dimension' should be safe.
        (nw, nh) = applySizeHintsContents sh (fi w + dx, fi h + dy)
        nx :: Rational
        nx = fi (ax * w + nw * (fi x - ax)) / fi w
        ny :: Rational
        ny = fi (ay * h + nh * (fi y - ay)) / fi h

keysResizeWindow' :: SizeHints -> P -> D -> ChangeDim -> G -> (P,D)
keysResizeWindow' sh (x,y) (w,h) (dx,dy) (gx, gy) = ((nx, ny), (nw, nh))
    where
        (nw, nh) = applySizeHintsContents sh (fi w + dx, fi h + dy)
        nx = round $ fi x + gx * fi w - gx * fi nw
        ny = round $ fi y + gy * fi h - gy * fi nh

keysMoveResize :: (SizeHints -> P -> D -> a -> b -> (P,D)) -> a -> b -> Window -> X ()
keysMoveResize f move resize w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    sh <- io $ getWMNormalHints d w
    let wa_dim = (fi $ wa_width wa, fi $ wa_height wa)
        wa_pos = (fi $ wa_x wa, fi $ wa_y wa)
        (wn_pos, wn_dim) = f sh wa_pos wa_dim move resize
    io $ resizeWindow d w `uncurry` wn_dim
    io $ moveWindow d w `uncurry` wn_pos
    float w
