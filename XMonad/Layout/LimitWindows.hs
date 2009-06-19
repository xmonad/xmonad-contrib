{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LimitWindows
-- Copyright   :  (c) 2009 Adam Vogt
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that limits the number of windows that can be shown.
--
-----------------------------------------------------------------------------

module XMonad.Layout.LimitWindows (limitWindows,limitSlice) where

import XMonad.Layout.LayoutModifier
import XMonad
import qualified XMonad.StackSet as W

-- | Only display the first @n@ windows.
limitWindows :: Int -> l a -> ModifiedLayout LimitWindows l a
limitWindows n = ModifiedLayout (LimitWindows FirstN n)

-- | Only display @n@ windows around the focused window. This makes sense with
-- layouts that arrange windows linearily, like 'XMonad.Layout.Layout.Accordion'.
limitSlice :: Int -> l a -> ModifiedLayout LimitWindows l a
limitSlice n = ModifiedLayout (LimitWindows Slice n)

data LimitWindows a = LimitWindows SliceStyle Int deriving (Read,Show)

data SliceStyle = FirstN | Slice deriving (Read,Show)

-- do the runLayout call in an environment with only the windows chosen by f ... ?
instance LayoutModifier LimitWindows a where
     modifyLayout (LimitWindows style n) ws r =
        runLayout ws { W.stack = f n `fmap` W.stack ws } r
      where f = case style of
                    FirstN -> firstN
                    Slice -> slice

firstN ::  Int -> W.Stack a -> W.Stack a
firstN n st = W.Stack f (reverse u) d
    where (u,f:d) = splitAt (min (n-1) $ length $ W.up st)
                    $ take n $ W.integrate st

slice ::  Int -> W.Stack t -> W.Stack t
slice n (W.Stack f u d) =
        W.Stack f (take (nu + unusedD) u)
                  (take (nd + unusedU) d)
    where unusedD = max 0 $ nd - length d
          unusedU = max 0 $ nu - length u
          nd = div (n - 1) 2
          nu = uncurry (+) $ divMod (n - 1) 2
