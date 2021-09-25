{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Column
-- Description :  Layout that places all windows in one column.
-- Copyright   :  (c) 2009 Ilya Portnov
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides Column layout that places all windows in one column. Windows
-- heights are calculated from equation: H1/H2 = H2/H3 = ... = q, where q is
-- given. With Shrink/Expand messages you can change the q value.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Column (
                             -- * Usage
                             -- $usage
                             Column (..)
                            ) where
import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- This module defines layot named Column. It places all windows in one
-- column. Windows heights are calculated from equation: H1/H2 = H2/H3 = ... =
-- q, where `q' is given (thus, windows heights are members of geometric
-- progression). With Shrink/Expand messages one can change the `q' value.
--
-- You can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.Column
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = Column 1.6 ||| ...
--
-- In this example, each next window will have height 1.6 times less then
-- previous window.

newtype Column a = Column Float deriving (Read,Show)

instance LayoutClass Column a where
    pureLayout = columnLayout
    pureMessage = columnMessage

columnMessage :: Column a -> SomeMessage -> Maybe (Column a)
columnMessage (Column q) m = fmap resize (fromMessage m)
    where resize Shrink = Column (q-0.1)
          resize Expand = Column (q+0.1)

columnLayout :: Column a -> Rectangle -> W.Stack a -> [(a,Rectangle)]
columnLayout (Column q) rect stack = zip ws rects
    where ws = W.integrate stack
          n = length ws
          heights = map (xn n rect q) [1..n]
          ys = [fromIntegral $ sum $ take k heights | k <- [0..n-1]]
          rects = zipWith (curry (mkRect rect)) heights ys

mkRect :: Rectangle -> (Dimension,Position) -> Rectangle
mkRect (Rectangle xs ys ws _) (h,y) = Rectangle xs (ys+fromIntegral y) ws h

xn :: Int -> Rectangle -> Float -> Int -> Dimension
xn n (Rectangle _ _ _ h) q k = if q==1 then
                                  h `div` fromIntegral n
                               else
                                  round (fromIntegral h*q^(n-k)*(1-q)/(1-q^n))
