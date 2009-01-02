{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ThreeColumnsMiddle
-- Copyright   :  (c) Carsten Otto <xmonad@c-otto.de>,
--                based on ThreeColumns (c) Kai Grossjohann <kai@emptydomain.de>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  ?
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout similar to tall but with three columns, where the main window is
-- in the middle. With 2560x1600 pixels this layout can be used for a huge
-- main window and up to six reasonable sized slave windows.
--
-- > Screenshot: http://server.c-otto.de/xmonad/ThreeColumnsMiddle.png
--
-----------------------------------------------------------------------------

module XMonad.Layout.ThreeColumnsMiddle (
                              -- * Usage
                              -- $usage
                              ThreeColMid(..)
                             ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Ratio

import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ThreeColumnsMiddle
--
-- Then edit your @layoutHook@ by adding the ThreeColMid layout:
--
-- > myLayouts = ThreeColMid 1 (3/100) (1/2) ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- The first argument specifies how many windows appear in the main window.
-- The second argument specifies how much the main window size changes when resizing.
-- The third argument specifies the initial size of the main window as a fraction of
-- total screen size.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data ThreeColMid a = ThreeColMid !Int !Rational !Rational deriving (Show,Read)

instance LayoutClass ThreeColMid a where
    doLayout (ThreeColMid nmaster _ frac) r =
        return . (\x->(x,Nothing)) .
        ap zip (tile3 frac r nmaster . length) . W.integrate
    handleMessage (ThreeColMid nmaster delta frac) m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = ThreeColMid nmaster delta (max 0 $ frac-delta)
                  resize Expand = ThreeColMid nmaster delta (min 1 $ frac+delta)
                  incmastern (IncMasterN d) = ThreeColMid (max 0 (nmaster+d)) delta frac
    description _ = "ThreeColMid"

-- | tile3.  Compute window positions using 3 panes
tile3 :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 f r nmaster n 
-- split horizontally, if there are very few windows (only the main screen is used)
    | n <= nmaster || nmaster == 0 = splitHorizontally n r

-- one window more than the master window can hold (the additional window is shown right of the main screen)
    | n == nmaster+1 = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2

-- many windows (the main windows are shown in the center, all other windows are shown left and right of it)
    | otherwise = splitVertically nmaster r1 ++ splitVertically nleft r2 ++ splitVertically nright r3
  where (r1, r2, r3) = split3HorizontallyBy f r
        (s1, s2) = splitHorizontallyBy f r
        nslave = (n - nmaster)
        nleft = ceiling (nslave % 2)
        nright = (n - nmaster - nleft)

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle (sx + fromIntegral leftw) sy midw sh
    , Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw + fromIntegral midw) sy rightw sh )
  where midw = ceiling $ fromIntegral sw * f
        leftw = ceiling ( (sw - midw) % 2 )
        rightw = sw - leftw - midw
