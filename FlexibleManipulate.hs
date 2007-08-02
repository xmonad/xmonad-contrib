{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.FlexibleManipulate
-- Copyright   :  (c) Michael Sloan
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  <mgsloan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Lets you move and resize floating windows without warping the mouse.
--
-----------------------------------------------------------------------------

-- Based on the FlexibleResize code by Lukas Mai (Mauke)

module XMonadContrib.FlexibleManipulate (
	-- * Usage
	-- $usage
	mouseWindow, discrete, linear, resize, position
) where

import XMonad
import Operations
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- Add this import to your Config.hs file:
--
-- > import qualified XMonadContrib.FlexibleManipulate as Flex
--
-- Set one of the mouse button bindings up like this:
-- > mouseBindings = M.fromList
-- >     [ ((modMask, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w)) ], ...
--
-- Flex.linear indicates that positions between the edges and the middle
--    indicate a combination scale/position.
-- Flex.discrete indicates that there are discrete pick regions. (window
--    is divided by thirds for each axis)
-- Flex.resize performs only resize of the window, based on which quadrant
--    the mouse is in
-- Flex.position is similar to the builtin mouseMoveWindow
--
-- You can also write your own function for this parameter. It should take
-- a value between 0 and 1 indicating position, and return a value indicating
-- the corresponding position if plain Flex.linear was used.

discrete, linear, resize, position :: Double -> Double

discrete x | x < 0.33 = 0
           | x > 0.66 = 1
           | otherwise = 0.5

linear = id

resize x = if x < 0.5 then 0 else 1
position = const 0.5

mouseWindow :: (Double -> Double) -> Window -> X ()
mouseWindow f w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    [wpos, wsize] <- io $ getWindowAttributes d w >>= return . winAttrs
    sh <- io $ getWMNormalHints d w
    pointer <- io $ queryPointer d w >>= return . pointerPos

    let uv = (pointer - wpos) / wsize
        fc = mapP f uv
        mul = mapP (\x -> 2 - 2 * abs(x - 0.5)) fc --Fudge factors: interpolation between 1 when on edge, 2 in middle
        atl = ((1, 1) - fc) * mul
        abr = fc * mul
    mouseDrag $ \(_, _, _, _, _, ex, ey, _, _, _) -> do
        let offset = (fromIntegral ex, fromIntegral ey) - pointer
            npos = wpos + offset * atl
            nbr = (wpos + wsize) + offset * abr
            ntl = minP (nbr - (32, 32)) npos    --minimum size
            nwidth = applySizeHints sh $ mapP round (nbr - ntl)
        moveResizeWindow d w (round $ fst ntl) (round $ snd ntl) `uncurry` nwidth

    float w
    
  where
    pointerPos (_,_,_,px,py,_,_,_) = (fromIntegral px,fromIntegral py) :: Pnt
    winAttrs :: WindowAttributes -> [Pnt]
    winAttrs x = pairUp $ map (fromIntegral . ($ x)) [wa_x, wa_y, wa_width, wa_height]


-- I'd rather I didn't have to do this, but I hate writing component 2d math
type Pnt = (Double, Double)

pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:xs) = (x, y) : (pairUp xs)

mapP :: (a -> b) -> (a, a) -> (b, b)
mapP f (x, y) = (f x, f y)
zipP :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c) 
zipP f (ax,ay) (bx,by) = (f ax bx, f ay by)

minP :: Ord a => (a,a) -> (a,a) -> (a,a)
minP = zipP min

instance Num Pnt where
    (+) = zipP (+)
    (-) = zipP (-)
    (*) = zipP (*)
    abs = mapP abs
    signum = mapP signum
    fromInteger = const undefined

instance Fractional Pnt where
    fromRational = const undefined
    recip = mapP recip
