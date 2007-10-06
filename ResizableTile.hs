{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ResizableTile
-- Copyright   :  (c) MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- More useful tiled layout that allows you to change a width/height of window.
--
-----------------------------------------------------------------------------

module XMonadContrib.ResizableTile (ResizableTall(..), MirrorResize(..)) where

import XMonad
import Operations (Resize(..), IncMasterN(..))
import qualified StackSet as W
import Graphics.X11.Xlib
import Control.Monad.State
import Control.Monad

-- $usage
--
-- To use, modify your Config.hs to:
--
-- >    import XMonadContrib.ResizableTile
--
-- and add a keybinding:
--
-- >    , ((modMask,               xK_a     ), sendMessage MirrorShrink)
-- >    , ((modMask,               xK_z     ), sendMessage MirrorExpand)
--
-- and redefine "tiled" as:
--
-- >     tiled   = ResizableTall nmaster delta ratio []

data MirrorResize = MirrorShrink | MirrorExpand deriving Typeable
instance Message MirrorResize

data ResizableTall a = ResizableTall Int Rational Rational [Rational] deriving (Show, Read)
instance LayoutClass ResizableTall a where
    doLayout (ResizableTall nmaster _ frac mfrac) r =
        return . (\x->(x,Nothing)) .
        ap zip (tile frac (mfrac ++ repeat 1) r nmaster . length) . W.integrate
    handleMessage (ResizableTall nmaster delta frac mfrac) m =
        do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
           case ms of
             Nothing -> return Nothing
             Just s -> return $ msum [fmap resize (fromMessage m)
                                     ,fmap (\x -> mresize x s) (fromMessage m)
                                     ,fmap incmastern (fromMessage m)]
        where resize Shrink = ResizableTall nmaster delta (max 0 $ frac-delta) mfrac
              resize Expand = ResizableTall nmaster delta (min 1 $ frac+delta) mfrac
              mresize MirrorShrink s = mresize' s delta
              mresize MirrorExpand s = mresize' s (0-delta)
              mresize' s d = let n = length $ W.up s
                                 total = n + (length $ W.down s) + 1
                                 pos = if n == (nmaster-1) || n == (total-1) then n-1 else n
                                 mfrac' = modifymfrac (mfrac ++ repeat 1) d pos
                             in ResizableTall nmaster delta frac $ take total mfrac'
              modifymfrac [] _ _ = []
              modifymfrac (f:fx) d n | n == 0    = f+d : fx
                                     | otherwise = f : modifymfrac fx d (n-1)
              incmastern (IncMasterN d) = ResizableTall (max 0 (nmaster+d)) delta frac mfrac
    description _ = "ResizableTall"

tile :: Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile f mf r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically mf n r
    else splitVertically mf nmaster r1 ++ splitVertically (drop nmaster mf) (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r = [r]
splitVertically _ n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically fx (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = floor $ fromIntegral (sh `div` fromIntegral n) * f --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f
