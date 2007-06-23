{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Magnifier
-- Copyright   :  (c) Peter De Wachter 2007
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Screenshot  :  <http://caladan.rave.org/magnifier.png>
--
-- This layout hack increases the size of the window that has focus.
--
-----------------------------------------------------------------------------


module XMonadContrib.Magnifier (
    -- * Usage
    -- $usage
    magnifier, magnifier') where

import Graphics.X11.Xlib
import XMonad
import StackSet

-- $usage
-- > import XMonadContrib.Magnifier
-- > defaultLayouts = [ magnifier tiled , magnifier $ mirror tiled ]

-- | Increase the size of the window that has focus, unless it is the master window.
magnifier :: Eq a => Layout a -> Layout a
magnifier l = l { doLayout = \r s -> unlessMaster applyMagnifier r s `fmap` doLayout l r s
                , modifyLayout = \x -> fmap magnifier `fmap` modifyLayout l x }

-- | Increase the size of the window that has focus, even if it is the master window.
magnifier' :: Eq a => Layout a -> Layout a
magnifier' l = l { doLayout = \r s -> applyMagnifier r s `fmap` doLayout l r s
                 , modifyLayout = \x -> fmap magnifier' `fmap` modifyLayout l x }


type DoLayout = Eq a => Rectangle -> Stack a -> [(a, Rectangle)] -> [(a, Rectangle)]

unlessMaster :: DoLayout -> DoLayout
unlessMaster f r s = if null (up s) then id else f r s

applyMagnifier :: DoLayout
applyMagnifier r s = reverse . foldr accumulate []
    where accumulate (w,wr) ws | w == focus s = ws ++ [(w, shrink r $ magnify wr)]
                               | otherwise    = (w,wr) : ws

magnify :: Rectangle -> Rectangle
magnify (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = x - fromIntegral (w' - w) `div` 2
          y' = y - fromIntegral (h' - h) `div` 2
          w' = round $ fromIntegral w * zoom
          h' = round $ fromIntegral h * zoom
          zoom = 1.5 :: Double

shrink :: Rectangle -> Rectangle -> Rectangle
shrink (Rectangle sx sy sw sh) (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = max sx x 
          y' = max sy y
          w' = min w (fromIntegral sx + sw - fromIntegral x')
          h' = min h (fromIntegral sy + sh - fromIntegral y')
