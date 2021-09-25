{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ResizableTile
-- Description :  More useful tiled layout that allows you to change a width\/height of window.
-- Copyright   :  (c) MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- More useful tiled layout that allows you to change a width\/height of window.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ResizableTile (
                                    -- * Usage
                                    -- $usage
                                    ResizableTall(..), MirrorResize(..)
                                   ) where

import XMonad hiding (tile, splitVertically, splitHorizontallyBy)
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ResizableTile
--
-- Then edit your @layoutHook@ by adding the ResizableTile layout:
--
-- > myLayout =  ResizableTall 1 (3/100) (1/2) [] ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You may also want to add the following key bindings:
--
-- > , ((modm,               xK_a), sendMessage MirrorShrink)
-- > , ((modm,               xK_z), sendMessage MirrorExpand)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data MirrorResize = MirrorShrink | MirrorExpand
instance Message MirrorResize

data ResizableTall a = ResizableTall
    { _nmaster :: Int       -- ^ number of master windows
    , _delta  :: Rational   -- ^ change when resizing by 'Shrink', 'Expand',
                            -- 'MirrorShrink', 'MirrorExpand'
    , _frac   :: Rational   -- ^ width of master
    , _slaves :: [Rational] -- ^ fraction to multiply the window
                            -- height that would be given when divided equally.
                            --
                            -- slave windows are assigned their modified
                            -- heights in order, from top to bottom
                            --
                            -- unspecified values are replaced by 1
    } deriving (Show, Read)

instance LayoutClass ResizableTall a where
    doLayout (ResizableTall nmaster _ frac mfrac) r =
        return . (, Nothing) .
        ap zip (tile frac (mfrac ++ repeat 1) r nmaster . length) . W.integrate
    handleMessage (ResizableTall nmaster delta frac mfrac) m =
        do ms <- W.stack . W.workspace . W.current <$> gets windowset
           fs <- M.keys . W.floating <$> gets windowset
           return $ ms >>= unfloat fs >>= handleMesg
        where handleMesg s = msum [fmap resize (fromMessage m)
                                  ,fmap (`mresize` s) (fromMessage m)
                                  ,fmap incmastern (fromMessage m)]
              unfloat fs s = if W.focus s `elem` fs
                               then Nothing
                               else Just (s { W.up = W.up s \\ fs
                                            , W.down = W.down s \\ fs })
              resize Shrink = ResizableTall nmaster delta (max 0 $ frac-delta) mfrac
              resize Expand = ResizableTall nmaster delta (min 1 $ frac+delta) mfrac
              mresize MirrorShrink s = mresize' s delta
              mresize MirrorExpand s = mresize' s (negate delta)
              mresize' s d = let n = length $ W.up s
                                 total = n + length (W.down s) + 1
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
  where smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f) --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f
