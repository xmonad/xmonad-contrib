{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiDishes
-- Description :  A layout stacking groups of extra windows underneath the master windows.
-- Copyright   :  (c) Jeremy Apthorp, Nathan Fairhurst
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Nathan Fairhurst <nathan.p3pictures@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- MultiDishes is a layout that stacks groups of extra windows underneath
-- the master windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MultiDishes (
                              -- * Usage
                              -- $usage
                              MultiDishes (..)
                            ) where

import XMonad
import XMonad.StackSet (integrate)
import XMonad.Prelude (ap)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MultiDishes
--
-- Then edit your @layoutHook@ by adding the MultiDishes layout:
--
-- > myLayout = MultiDishes 2 3 (1/6) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- This is based on the Layout Dishes, but accepts another parameter for
-- the maximum number of dishes allowed within a stack.
--
-- > MultiDishes x 1 y
-- is equivalent to
-- > Dishes x y
--
-- The stack with the fewest dishes is always on top, so 4 windows
-- with the layout `MultiDishes 1 2 (1/5)` would look like this:
--
-- > _________
-- > |       |
-- > |   M   |
-- > |_______|
-- > |_______|
-- > |___|___|
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data MultiDishes a = MultiDishes Int Int Rational deriving (Show, Read)
instance LayoutClass MultiDishes a where
    pureLayout (MultiDishes nmaster dishesPerStack h) r =
        ap zip (multiDishes h r nmaster dishesPerStack . length) . integrate
    pureMessage (MultiDishes nmaster dishesPerStack h) m = fmap incmastern (fromMessage m)
        where incmastern (IncMasterN d) = MultiDishes (max 0 (nmaster+d)) dishesPerStack h

multiDishes :: Rational -> Rectangle -> Int -> Int -> Int -> [Rectangle]
multiDishes h s nmaster dishesPerStack n = if n <= nmaster
                        then splitHorizontally n s
                        else ws
 where
    (filledDishStackCount, remainder) =
      (n - nmaster) `quotRem` max 1 dishesPerStack

    (firstDepth, dishStackCount) =
      if remainder == 0 then
        (dishesPerStack, filledDishStackCount)
      else
        (remainder, filledDishStackCount + 1)

    (masterRect, dishesRect) =
      splitVerticallyBy (1 - fromIntegral dishStackCount * h) s

    dishStackRects =
      splitVertically dishStackCount dishesRect

    allDishRects = case dishStackRects of
      (firstStack:bottomDishStacks) ->
        splitHorizontally firstDepth firstStack ++ (bottomDishStacks >>= splitHorizontally dishesPerStack)
      [] -> []

    ws =
      splitHorizontally nmaster masterRect ++ allDishRects
