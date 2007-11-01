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

import Graphics.X11.Xlib (Window, Rectangle(..))
import XMonad
import XMonad.StackSet
import XMonadContrib.LayoutHelpers

-- $usage
-- > import XMonadContrib.Magnifier
-- > layouts = [ magnifier tiled , magnifier $ mirror tiled ]

-- %import XMonadContrib.Magnifier
-- %layout , magnifier tiled
-- %layout , magnifier $ mirror tiled

-- | Increase the size of the window that has focus, unless it is the master window.
magnifier :: Layout Window -> Layout Window
magnifier = layoutModify (unlessMaster applyMagnifier) idModMod

-- | Increase the size of the window that has focus, even if it is the master window.
magnifier' :: Layout Window -> Layout Window
magnifier' = layoutModify applyMagnifier idModMod

unlessMaster :: ModDo Window -> ModDo Window
unlessMaster mainmod r s wrs = if null (up s) then return (wrs, Nothing)
                                              else mainmod r s wrs

applyMagnifier :: ModDo Window
applyMagnifier r _ wrs = do focused <- withWindowSet (return . peek)
                            let mag (w,wr) ws | focused == Just w = ws ++ [(w, shrink r $ magnify wr)]
                                              | otherwise         = (w,wr) : ws
                            return (reverse $ foldr mag [] wrs, Nothing)

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
