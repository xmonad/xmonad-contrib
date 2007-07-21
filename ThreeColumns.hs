-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ThreeColumns
-- Copyright   :  (c) Kai Grossjohann <kai@emptydomain.de>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  ?
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout similar to tall but with three columns.
--
-----------------------------------------------------------------------------

module XMonadContrib.ThreeColumns (
                              -- * Usage
                              -- $usage
                              threeCol
                             ) where

import XMonad
import qualified StackSet as W
import Operations ( Resize(..), IncMasterN(..), splitVertically, tall, splitHorizontallyBy )

import Data.Ratio

--import Control.Monad.State
import Control.Monad.Reader

import Graphics.X11.Xlib

-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.ThreeColumns
--
--  and add, to the list of layouts:
--
-- > threeCol

threeCol :: Int -> Rational -> Rational -> Layout a
threeCol nmaster delta frac =
    Layout { doLayout     = \r -> return . (\x->(x,Nothing)) .
                                  ap zip (tile3 frac r nmaster . length) . W.integrate
           , modifyLayout = \m -> return $ msum [fmap resize     (fromMessage m)
                                                ,fmap incmastern (fromMessage m)] }

    where resize Shrink = tall nmaster delta (max 0 $ frac-delta)
          resize Expand = tall nmaster delta (min 1 $ frac+delta)
          incmastern (IncMasterN d) = tall (max 0 (nmaster+d)) delta frac

-- | tile3.  Compute window positions using 3 panes
tile3 :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 f r nmaster n 
    | n <= nmaster || nmaster == 0 = splitVertically n r
    | n <= nmaster+1 = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2
    | otherwise = splitVertically nmaster r1 ++ splitVertically nmid r2 ++ splitVertically nright r3
  where (r1, r2, r3) = split3HorizontallyBy f r
        (s1, s2) = splitHorizontallyBy f r
        nslave = (n - nmaster)
        nmid = ceiling (nslave % 2)
        nright = (n - nmaster - nmid)

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy midw sh
    , Rectangle (sx + fromIntegral leftw + fromIntegral midw) sy rightw sh )
  where leftw = ceiling $ fromIntegral sw * (2/3) * f
        midw = ceiling ( (sw - leftw) % 2 )
        rightw = sw - leftw - midw
