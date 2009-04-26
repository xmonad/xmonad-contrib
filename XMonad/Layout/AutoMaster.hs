{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.AutoMaster
-- Copyright   :  (c) 2009 Ilya Portnov
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides layout modifier AutoMaster. It separates screen in two parts - 
-- master and slave. Size of slave area automatically changes depending on
-- number of slave windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.AutoMaster (
                             -- * Usage
                             -- $usage
                             autoMaster
                            ) where
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier

-- $usage
-- This module defines layout modifier named autoMaster. It separates
-- screen in two parts - master and slave. Master windows are arranged
-- in one row, in slave area underlying layout is run. Size of slave area
-- automatically increases when number of slave windows is increasing.
--
-- You can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.AutoMaster
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = autoMaster 1 (1/100) Grid ||| ...
--
-- In this example, master area by default contains 1 window (you can
-- change this number in runtime with usual IncMasterN message), changing
-- slave area size with 1/100 on each Shrink/Expand message.

-- | Data type for layout modifier
data AutoMaster a = AutoMaster Int Float Float
    deriving (Read,Show)

instance LayoutModifier AutoMaster Window where
    modifyLayout (AutoMaster k bias _) = autoLayout k bias 
    pureMess = autoMess

-- | Handle Shrink/Expand and IncMasterN messages
autoMess :: AutoMaster a -> SomeMessage -> Maybe (AutoMaster a)
autoMess (AutoMaster k bias delta) m = msum [fmap resize (fromMessage m),
                                             fmap incmastern (fromMessage m)]
    where incmastern (IncMasterN d) = AutoMaster (max 1 (k+d)) bias delta
          resize Expand = AutoMaster k (min ( 0.4)  $ bias+delta) delta
          resize Shrink = AutoMaster k (max (-0.4)  $ bias-delta) delta

-- | Main layout function
autoLayout :: (LayoutClass l Window) =>
              Int ->
              Float ->
              W.Workspace WorkspaceId (l Window) Window
              -> Rectangle
              -> X ([(Window, Rectangle)], Maybe (l Window))
autoLayout k bias wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' stack
    let n = length ws
    if null ws then
        runLayout wksp rect
        else do
          if (n<=k) then
              return ((divideRow rect ws),Nothing)
              else do
              let master = take k ws
              let filtStack = stack >>= W.filter (\w -> not (w `elem` master))
              wrs <- runLayout (wksp {W.stack = filtStack}) (slaveRect rect n bias)
              return ((divideRow (masterRect rect n bias) master) ++ (fst wrs),
                      snd wrs)

-- | Calculates height of master area, depending on number of windows.
masterHeight :: Int -> Float -> Float
masterHeight n bias = (calcHeight n) + bias
    where calcHeight :: Int -> Float
          calcHeight 1 = 1.0
          calcHeight m = if (m<9) then (43/45) - (fromIntegral m)*(7/90) else (1/3)

-- | Rectangle for master area
masterRect :: Rectangle -> Int -> Float -> Rectangle
masterRect (Rectangle sx sy sw sh) n bias = Rectangle sx sy sw h
    where h = round $ (fromIntegral sh)*(masterHeight n bias)

-- | Rectangle for slave area
slaveRect :: Rectangle -> Int -> Float -> Rectangle
slaveRect (Rectangle sx sy sw sh) n bias = Rectangle sx (sy+(fromIntegral mh)) sw h
    where mh = round $ (fromIntegral sh)*(masterHeight n bias)
          h  = round $ (fromIntegral sh)*(1-masterHeight n bias)

-- | Divide rectangle between windows 
divideRow :: Rectangle -> [a] -> [(a, Rectangle)]
divideRow (Rectangle x y w h) ws = zip ws rects
    where n = length ws
          oneW = fromIntegral w `div` n
          oneRect = Rectangle x y (fromIntegral oneW) h
          rects = take n $ iterate (shiftR (fromIntegral oneW)) oneRect

-- | Shift rectangle right 
shiftR :: Position -> Rectangle -> Rectangle
shiftR s (Rectangle x y w h) = Rectangle (x+s) y w h

-- | User interface function
autoMaster :: LayoutClass l a =>
              Int ->      -- Number of master windows
              Float ->    -- Step for which to increment/decrement master area size with Shrink/Expand
              l a ->
              ModifiedLayout AutoMaster l a
autoMaster nmaster delta = ModifiedLayout (AutoMaster nmaster 0 delta)

