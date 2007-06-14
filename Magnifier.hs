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
-- Screenshot  :  http://caladan.rave.org/magnifier.png
--
-- This layout hack increases the size of the window that has focus.
-- The master window is left alone. (Maybe that should be an option.)
--
--
-----------------------------------------------------------------------------


module XMonadContrib.Magnifier (magnifier) where

import Graphics.X11.Xlib
import XMonad
import StackSet

magnifier :: Layout -> Layout
magnifier l = l { doLayout = \r s -> applyMagnifier r s `fmap` doLayout l r s
                , modifyLayout = \x -> fmap magnifier `fmap` modifyLayout l x }

applyMagnifier :: Rectangle -> Stack Window -> [(Window, Rectangle)] -> [(Window, Rectangle)]
applyMagnifier r s | null (up s) = id  -- don't change the master window
                   | otherwise   = map $ \(w,wr) -> if w == focus s then (w, shrink r $ magnify wr) else (w, wr)

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
