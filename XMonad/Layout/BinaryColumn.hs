{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BinaryColumn
-- Copyright   :  (c) 2009 Ilya Portnov, (c) 2018 Campbell Barton
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Campbell Barton <ideasman42@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides Column layout that places all windows in one column.
-- Each window is half the height of the previous,
-- except for the last pair of windows.
--
-- Note: Originally based on 'XMonad.Layout.Column' with changes:
--
-- * Adding/removing windows doesn't resize all other windows.
-- (last window pair exception).
-- * Minimum window height option.
--
-----------------------------------------------------------------------------

module XMonad.Layout.BinaryColumn (
                             -- * Usage
                             -- $usage
                             BinaryColumn (..)
                            ) where
import XMonad
import qualified XMonad.StackSet
import qualified Data.List

-- $usage
-- This module defines layout named BinaryColumn.
-- It places all windows in one column.
-- Windows heights are calculated to prevent window resizing whenever
-- a window is added or removed.
-- This is done by keeping the last two windows in the stack the same height.
--
-- You can use this module by adding following in your @xmonad.hs@:
--
-- > import XMonad.Layout.BinaryColumn
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = BinaryColumn 1.0 32 ||| ...
--
-- The first value causes the master window to take exactly half of the screen,
-- the second ensures that windows are no less than 32 pixels tall.
--
-- Shrink/Expand can be used to adjust the first value by increments of 0.1.
--
-- * 2.0 uses all space for the master window
-- (minus the space for windows which get their fixed height).
-- * 0.0 gives an evenly spaced grid.
-- Negative values reverse the sizes so the last
-- window in the stack becomes larger.
--

data BinaryColumn a = BinaryColumn Float Int
  deriving (Read, Show)

instance XMonad.LayoutClass BinaryColumn a where
  pureLayout = columnLayout
  pureMessage = columnMessage

columnMessage :: BinaryColumn a -> SomeMessage -> Maybe (BinaryColumn a)
columnMessage (BinaryColumn q min_size) m = fmap resize (fromMessage m)
  where
    resize Shrink = BinaryColumn (max (-2.0) (q - 0.1)) min_size
    resize Expand = BinaryColumn (min   2.0  (q + 0.1)) min_size

columnLayout :: BinaryColumn a
  -> XMonad.Rectangle
  -> XMonad.StackSet.Stack a
  -> [(a, XMonad.Rectangle)]
columnLayout (BinaryColumn scale min_size) rect stack = zip ws rects
  where
    ws = XMonad.StackSet.integrate stack
    n = length ws
    scale_abs = abs scale
    heights_noflip =
      let
        -- Regular case: check for min size.
        f n size div False = let
          n_fl = (fromIntegral n)
          n_prev_fl = (fromIntegral (n + 1))
          div_test = min (div) (n_prev_fl)
          value_test = (toInteger (round ((fromIntegral size) / div_test)))
          value_max = size - (toInteger (min_size * n))
          (value, divide_next, no_room) =
            if value_test < value_max then
              (value_test, div, False)
            else
              (value_max, n_fl, True)
          size_next = size - value
          n_next = n - 1
          in value
          : f n_next size_next divide_next no_room
        -- Fallback case: when windows have reached min size
        -- simply create an even grid with the remaining space.
        f n size div True = let
          n_fl = (fromIntegral n)
          value_even = ((fromIntegral size) / div)
          value = (toInteger (round value_even))

          n_next = n - 1
          size_next = size - value
          divide_next = n_fl
          in value
          : f n_next size_next n_fl True
        -- Last item: included twice.
        f 0 size div no_room_prev =
          [size];
      in f
         n_init size_init divide_init False
      where
        n_init = n - 1
        size_init = (toInteger (rect_height rect))
        divide_init =
          if scale_abs == 0.0 then
            (fromIntegral n)
          else
            (1.0 / (0.5 * scale_abs))

    heights =
      if (scale < 0.0) then
        Data.List.reverse (take n heights_noflip)
      else
        heights_noflip

    ys = [fromIntegral $ sum $ take k heights | k <- [0..n - 1]]
    rects = map (mkRect rect) $ zip heights ys

mkRect :: XMonad.Rectangle
  -> (Integer,XMonad.Position)
  -> XMonad.Rectangle
mkRect (XMonad.Rectangle xs ys ws _) (h, y) =
  XMonad.Rectangle xs (ys + fromIntegral y) ws (fromInteger h)
