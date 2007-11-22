{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Magnifier
-- Copyright   :  (c) Peter De Wachter 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>,
--                andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- Screenshot  :  <http://caladan.rave.org/magnifier.png>
--
-- This is a layout modifier that will make a layout increase the size
-- of the window that has focus.
--
-----------------------------------------------------------------------------


module XMonad.Layout.Magnifier (
    -- * Usage
    -- $usage
    magnifier,
    magnifier') where

import Graphics.X11.Xlib (Window, Rectangle(..))
import XMonad
import XMonad.StackSet
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Magnifier
--
-- Then edit your @layoutHook@ by adding the Magnifier layout modifier
-- to some layout:
--
-- > myLayouts = magnifier (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad dafaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Increase the size of the window that has focus, unless it is the
-- master window.
magnifier :: l a -> ModifiedLayout Magnifier l a
magnifier = ModifiedLayout (M True)

-- | Increase the size of the window that has focus, even if it is the
-- master window.
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (M False)

data Magnifier a = M Bool deriving (Read, Show)

instance LayoutModifier Magnifier Window where
    modifierDescription (M b) = (if b then "" else "All") ++ "Magnifier"
    redoLayout          (M b) = if b
                                then unlessMaster applyMagnifier
                                else applyMagnifier

type NewLayout a = Rectangle -> Stack a -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe (Magnifier a))

unlessMaster :: NewLayout a -> NewLayout a
unlessMaster mainmod r s wrs = if null (up s) then return (wrs, Nothing)
                                              else mainmod r s wrs

applyMagnifier :: Rectangle -> t -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe a)
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
