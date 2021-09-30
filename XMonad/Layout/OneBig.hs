{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.OneBig
-- Description :  Place one window at top left corner, and other windows at the top.
-- Copyright   :  (c) 2009 Ilya Portnov
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides layout named OneBig. It places one (master) window at top left corner of screen, and other (slave) windows at top
--
-----------------------------------------------------------------------------

module XMonad.Layout.OneBig (
                             -- * Usage
                             -- $usage
                             OneBig (..)
                            ) where
import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module defines layout named OneBig. It places one (master)
-- window at top left, and other (slave) windows at right and at
-- bottom of master. It tries to give equal space for each slave
-- window.
--
-- You can use this module by adding following in your @xmonad.hs@:
--
-- > import XMonad.Layout.OneBig
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = OneBig (3/4) (3/4) ||| ...
--
-- In this example, master window will occupy 3/4 of screen width and
-- 3/4 of screen height.

-- | Data type for layout
data OneBig a = OneBig Float Float deriving (Read,Show)

instance LayoutClass OneBig a where
  pureLayout = oneBigLayout
  pureMessage = oneBigMessage

-- | Processes Shrink/Expand messages
oneBigMessage :: OneBig a -> SomeMessage -> Maybe (OneBig a)
oneBigMessage (OneBig cx cy) m = fmap resize (fromMessage m)
    where resize Shrink = OneBig (cx-delta) (cy-delta)
          resize Expand = OneBig (cx+delta) (cy+delta)
          delta = 3/100

-- | Main layout function
oneBigLayout :: OneBig a -> Rectangle -> W.Stack a -> [(a, Rectangle)]
oneBigLayout (OneBig cx cy) rect stack = [(master,masterRect)]
                                      ++ divideBottom bottomRect bottomWs
                                      ++ divideRight rightRect rightWs
      where ws = W.integrate stack
            n = length ws
            ht (Rectangle _ _ _ hh) = hh
            wd (Rectangle _ _ ww _) = ww
            h' = round (fromIntegral (ht rect)*cy)
            w = wd rect
            m = calcBottomWs n w h'
            master = head ws
            other  = tail ws
            bottomWs = take m other
            rightWs = drop m other
            masterRect = cmaster n m cx cy rect
            bottomRect = cbottom cy rect
            rightRect  = cright cx cy rect

-- | Calculate how many windows must be placed at bottom
calcBottomWs :: Int -> Dimension -> Dimension -> Int
calcBottomWs n w h' = case n of
                        1 -> 0
                        2 -> 1
                        3 -> 2
                        4 -> 2
                        _ -> fromIntegral w*(n-1) `div` fromIntegral (h'+fromIntegral w)

-- | Calculate rectangle for master window
cmaster:: Int -> Int -> Float -> Float -> Rectangle -> Rectangle
cmaster n m cx cy (Rectangle x y sw sh) = Rectangle x y w h
    where w = if n > m+1 then
                round (fromIntegral sw*cx)
              else
                sw
          h = if n > 1 then
                round (fromIntegral sh*cy)
              else
                sh

-- | Calculate rectangle for bottom windows
cbottom:: Float -> Rectangle -> Rectangle
cbottom cy (Rectangle sx sy sw sh) = Rectangle sx y sw h
    where h = round (fromIntegral sh*(1-cy))
          y = round (fromIntegral sh*cy+fromIntegral sy)

-- | Calculate rectangle for right windows
cright:: Float -> Float -> Rectangle -> Rectangle
cright cx cy (Rectangle sx sy sw sh) = Rectangle x sy w h
    where w = round (fromIntegral sw*(1-cx))
          x = round (fromIntegral sw*cx+fromIntegral sx)
          h = round (fromIntegral sh*cy)

-- | Divide bottom rectangle between windows
divideBottom :: Rectangle -> [a] -> [(a, Rectangle)]
divideBottom (Rectangle x y w h) ws = zip ws rects
    where n = length ws
          oneW = fromIntegral w `div` n
          oneRect = Rectangle x y (fromIntegral oneW) h
          rects = take n $ iterate (shiftR (fromIntegral oneW)) oneRect

-- | Divide right rectangle between windows
divideRight :: Rectangle -> [a] -> [(a, Rectangle)]
divideRight (Rectangle x y w h) ws = if n==0 then [] else zip ws rects
    where n = length ws
          oneH = fromIntegral h `div` n
          oneRect = Rectangle x y w (fromIntegral oneH)
          rects = take n $ iterate (shiftB (fromIntegral oneH)) oneRect

-- | Shift rectangle right
shiftR :: Position -> Rectangle -> Rectangle
shiftR s (Rectangle x y w h) = Rectangle (x+s) y w h

-- | Shift rectangle bottom
shiftB :: Position -> Rectangle -> Rectangle
shiftB s (Rectangle x y w h) = Rectangle x (y+s) w h
