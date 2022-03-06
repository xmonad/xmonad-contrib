{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ThreeColumns
-- Description :  A layout similar to @Tall@, but with three columns.
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
-----------------------------------------------------------------------------

module XMonad.Layout.ThreeColumns (
                              -- * Usage
                              -- $usage

                              -- * Screenshots
                              -- $screenshot
                              ThreeCol(..)
                             ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import Data.Ratio

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ThreeColumns
--
-- Then edit your @layoutHook@ by adding the ThreeCol layout:
--
-- > myLayout = ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the main
-- window. The second argument argument specifies the amount to resize while
-- resizing and the third argument specifies the initial size of the columns.
-- A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a stack column should occupy. If both stack columns are visible,
-- they always occupy the same amount of space.
--
-- The ThreeColMid variant places the main window between the stack columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- $screenshot
-- <<https://user-images.githubusercontent.com/50166980/156938482-ac38fdd7-eb94-4371-801b-e191cdb9a4ba.png>>

-- | Arguments are nmaster, delta, fraction
data ThreeCol a = ThreeColMid { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational}
                | ThreeCol    { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational}
    deriving (Show,Read)

instance LayoutClass ThreeCol a where
    pureLayout (ThreeCol n _ f) r    = doL False n f r
    pureLayout (ThreeColMid n _ f) r = doL True n f r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { threeColFrac = max (-0.5) $ f-d }
                  resize Expand = l { threeColFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { threeColNMaster = max 0 (n+x) }
                  n = threeColNMaster l
                  d = threeColDelta l
                  f = threeColFrac l
    description _ = "ThreeCol"

doL :: Bool-> Int-> Rational-> Rectangle-> W.Stack a-> [(a, Rectangle)]
doL m n f r = ap zip (tile3 m f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f r nmaster n
    | n <= nmaster || nmaster == 0 = splitVertically n r
    | n <= nmaster+1 = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2
    | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r2 ++ splitVertically nslave2 r3
        where (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (s1, s2) = splitHorizontallyBy (if f<0 then 1+f else f) r
              nslave = n - nmaster
              nslave1 = ceiling (nslave % 2)
              nslave2 = n - nmaster - nslave1

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( Rectangle (sx + fromIntegral r3w) sy r1w sh
         , Rectangle sx sy r3w sh
         , Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh )
    else ( Rectangle sx sy r1w sh
         , Rectangle (sx + fromIntegral r1w) sy r2w sh
         , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w
