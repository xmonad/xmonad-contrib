{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ThreeColumns
-- Copyright   :  (c) Kai Grossjohann <kai@emptydomain.de>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  ?
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout similar to tall but with three columns. With 2560x1600 pixels this
-- layout can be used for a huge main window and up to six reasonable sized
-- slave windows.
--
-- Screenshot: <http://server.c-otto.de/xmonad/ThreeColumnsMiddle.png>
--
-----------------------------------------------------------------------------

module XMonad.Layout.ThreeColumns (
                              -- * Usage
                              -- $usage
                              ThreeCol(..)
                             ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Ratio

import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ThreeColumns
--
-- Then edit your @layoutHook@ by adding the ThreeCol layout:
--
-- > myLayouts = ThreeCol False 1 (3/100) (1/2) ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- If the first argument is true, the main window is placed in the center
-- column. The second argument specifies hom many windows initially appear in
-- the main window. The third argument argument specifies the amount to resize
-- while resizing and the fourth argument specifies the initial size of the
-- columns. A positive size designates the fraction of the screen that the main
-- window should occupy, but if the size is negative the absolute value
-- designates the fraction a slave column should occupy. If both slave columns
-- are visible, they always occupy the same amount of space.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data ThreeCol a = ThreeCol !Bool !Int !Rational !Rational deriving (Show,Read)

instance LayoutClass ThreeCol a where
    doLayout (ThreeCol middle nmaster _ frac) r =
        return . (\x->(x,Nothing)) .
        ap zip (tile3 middle frac r nmaster . length) . W.integrate
    handleMessage (ThreeCol middle nmaster delta frac) m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = ThreeCol middle nmaster delta (max (-0.5) $ frac-delta)
                  resize Expand = ThreeCol middle nmaster delta (min 1 $ frac+delta)
                  incmastern (IncMasterN d) = ThreeCol middle (max 0 (nmaster+d)) delta frac
    description _ = "ThreeCol"

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
    | n <= nmaster || nmaster == 0 = splitVertically n r
    | n <= nmaster+1 = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2
    | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r2 ++ splitVertically nslave2 r3
        where (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (s1, s2) = splitHorizontallyBy (if f<0 then 1+f else f) r
              nslave = (n - nmaster)
              nslave1 = ceiling (nslave % 2)
              nslave2 = (n - nmaster - nslave1)

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( Rectangle (sx + fromIntegral r2w) sy r1w sh
         , Rectangle sx sy r2w sh
         , Rectangle (sx + fromIntegral r2w + fromIntegral r1w) sy r3w sh )
    else ( Rectangle sx sy r1w sh
         , Rectangle (sx + fromIntegral r1w) sy r2w sh
         , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w
