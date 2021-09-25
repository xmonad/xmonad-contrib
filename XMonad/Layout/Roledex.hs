{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Roledex
-- Description :  A completely pointless layout which acts like Microsoft's Flip 3D.
-- Copyright   :  (c) tim.thelion@gmail.com
-- License     :  BSD
--
-- Maintainer  :  tim.thelion@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a completely pointless layout which acts like Microsoft's Flip 3D
-----------------------------------------------------------------------------

module XMonad.Layout.Roledex (
    -- * Usage
    -- $usage

    -- * Screenshots
    -- $screenshot
    Roledex(Roledex)) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Ratio

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Roledex
--
-- Then edit your @layoutHook@ by adding the Roledex layout:
--
-- > myLayout =  Roledex ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- $screenshot
-- <<http://www.timthelion.com/rolodex.png>>

data Roledex a = Roledex deriving ( Show, Read )

instance LayoutClass Roledex Window where
    doLayout _ = roledexLayout

roledexLayout :: Eq a => Rectangle -> W.Stack a -> X ([(a, Rectangle)], Maybe (Roledex a))
roledexLayout sc ws = return ([(W.focus ws, mainPane)] ++
                              zip ups tops ++
                              reverse (zip dns bottoms)
                               ,Nothing)
 where ups    = W.up ws
       dns    = W.down ws
       c = length ups + length dns
       rect = fst $ splitHorizontallyBy (2%3 :: Ratio Int) $ fst (splitVerticallyBy (2%3 :: Ratio Int) sc)
       gw = div' (w - rw) (fromIntegral c)
            where
            (Rectangle _ _ w _) = sc
            (Rectangle _ _ rw _) = rect
       gh = div' (h - rh) (fromIntegral c)
            where
            (Rectangle _ _ _ h) = sc
            (Rectangle _ _ _ rh) = rect
       mainPane = mrect (gw * fromIntegral c) (gh * fromIntegral c) rect
       mrect  mx my (Rectangle x y w h) = Rectangle (x + fromIntegral mx) (y + fromIntegral my) w h
       tops    = map f $ cd c (length dns)
       bottoms = map f [0..(length dns)]
       f n = mrect (gw * fromIntegral n) (gh * fromIntegral n) rect
       cd n m = if n > m
                then (n - 1) : cd (n-1) m
                else []

div' :: Integral a => a -> a -> a
div' _ 0 = 0
div' n o = div n o
