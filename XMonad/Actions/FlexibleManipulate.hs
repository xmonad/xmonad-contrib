{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.FlexibleManipulate
-- Description :  Move and resize floating windows without warping the mouse.
-- Copyright   :  (c) Michael Sloan
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <mgsloan@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- Move and resize floating windows without warping the mouse.
--
-----------------------------------------------------------------------------

-- Based on the FlexibleResize code by Lukas Mai (mauke).

module XMonad.Actions.FlexibleManipulate (
        -- * Usage
        -- $usage
        mouseWindow, discrete, linear, resize, position
) where

import XMonad
import XMonad.Prelude ((<&>), fi)
import qualified Prelude as P
import Prelude (Double, Integer, Ord (..), const, fromIntegral, fst, id, otherwise, round, snd, uncurry, ($))

-- $usage
-- First, add this import to your @~\/.xmonad\/xmonad.hs@:
--
-- > import qualified XMonad.Actions.FlexibleManipulate as Flex
--
-- Now set up the desired mouse binding, for example:
--
-- >     , ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
--
-- * Flex.'linear' indicates that positions between the edges and the
--   middle indicate a combination scale\/position.
--
-- * Flex.'discrete' indicates that there are discrete pick
--   regions. (The window is divided by thirds for each axis.)
--
-- * Flex.'resize' performs only a resize of the window, based on which
--   quadrant the mouse is in.
--
-- * Flex.'position' is similar to the built-in
--   'XMonad.Operations.mouseMoveWindow'.
--
-- You can also write your own function for this parameter. It should take
-- a value between 0 and 1 indicating position, and return a value indicating
-- the corresponding position if plain Flex.'linear' was used.
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

discrete, linear, resize, position :: Double -> Double

-- | Manipulate the window based on discrete pick regions; the window
--   is divided into regions by thirds along each axis.
discrete x | x < 0.33 = 0
           | x > 0.66 = 1
           | otherwise = 0.5

-- | Scale\/reposition the window by factors obtained from the mouse
--   position by linear interpolation. Dragging precisely on a corner
--   resizes that corner; dragging precisely in the middle moves the
--   window without resizing; anything else is an interpolation
--   between the two.
linear = id

-- | Only resize the window, based on the window quadrant the mouse is in.
resize x = if x < 0.5 then 0 else 1

-- | Only reposition the window.
position = const 0.5

-- | Given an interpolation function, implement an appropriate window
--   manipulation action.
mouseWindow :: (Double -> Double) -> Window -> X ()
mouseWindow f w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    let wpos  = (fi (wa_x wa), fi (wa_y wa))
        wsize = (fi (wa_width wa), fi (wa_height wa))
    sh <- io $ getWMNormalHints d w
    pointer <- io $ queryPointer d w <&> pointerPos

    let uv = (pointer - wpos) / wsize
        fc = mapP f uv
        mul = mapP (\x -> 2 P.- 2 P.* P.abs(x P.- 0.5)) fc --Fudge factors: interpolation between 1 when on edge, 2 in middle
        atl = ((1, 1) - fc) * mul
        abr = fc * mul
    mouseDrag (\ex ey -> do
        let offset = (fromIntegral ex, fromIntegral ey) - pointer
            npos = wpos + offset * atl
            nbr = (wpos + wsize) + offset * abr
            ntl = minP (nbr - (32, 32)) npos    --minimum size
            nwidth = applySizeHintsContents sh $ mapP (round :: Double -> Integer) (nbr - ntl)
        io $ moveResizeWindow d w (round $ fst ntl) (round $ snd ntl) `uncurry` nwidth
        float w)
        (float w)

    float w

  where
    pointerPos (_,_,_,px,py,_,_,_) = (fromIntegral px,fromIntegral py) :: Pnt

-- I'd rather I didn't have to do this, but I hate writing component 2d math
type Pnt = (Double, Double)

mapP :: (a -> b) -> (a, a) -> (b, b)
mapP f (x, y) = (f x, f y)
zipP :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipP f (ax,ay) (bx,by) = (f ax bx, f ay by)

minP :: Ord a => (a,a) -> (a,a) -> (a,a)
minP = zipP min

infixl 6  +, -
infixl 7  *, /

(+), (-), (*) :: (P.Num a) => (a,a) -> (a,a) -> (a,a)
(+) = zipP (P.+)
(-) = zipP (P.-)
(*) = zipP (P.*)
(/) :: (P.Fractional a) => (a,a) -> (a,a) -> (a,a)
(/) = zipP (P./)
