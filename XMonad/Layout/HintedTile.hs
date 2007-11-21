{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.HintedTile
-- Copyright   :  (c) Peter De Wachter <pdewacht@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
--                Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A gapless tiled layout that attempts to obey window size hints,
-- rather than simply ignoring them.
--
-----------------------------------------------------------------------------

module XMonad.Layout.HintedTile (
                                 -- * Usage
                                 -- $usage
                                 tall, wide ) where

import XMonad
import XMonad.Layouts    ( Resize(..), IncMasterN(..) )
import XMonad.Operations ( applySizeHints             )
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad.Reader

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.HintedTile
--
-- Then edit your @layoutHook@ by adding the HintedTile layout:
--
-- > myLayouts = tall 1 0.1 0.5 ||| Full ||| etc..
-- > main = xmonad dafaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data HintedTile a =
    HT { nmaster     :: Int
       , delta, frac :: Rational
       , orientation :: Orientation
       } deriving ( Show, Read )

data Orientation = Wide | Tall deriving ( Show, Read )

tall, wide :: Int -> Rational -> Rational -> HintedTile Window
wide n d f = HT {nmaster = n, delta = d, frac = f, orientation = Tall }
tall n d f = HT {nmaster = n, delta = d, frac = f, orientation = Wide }

instance LayoutClass HintedTile Window where
    doLayout c rect w' = let w = W.integrate w'
                         in do { hints <- sequence (map getHints w)
                               ; b <- asks (borderWidth . config)
                               ; return (zip w (tiler b (frac c) rect `uncurry` splitAt (nmaster c) hints)
                                        , Nothing) }
        where
          (split, divide) =
              case orientation c of
                Wide -> (splitHorizontally, divideHorizontally)
                Tall -> (splitVertically,   divideVertically  )
          tiler b f r masters slaves =
              if null masters || null slaves
              then divide b (masters ++ slaves) r
              else split f r (divide b masters) (divide b slaves)

    pureMessage c m = fmap resize (fromMessage m) `mplus`
                      fmap incmastern (fromMessage m)
        where
          resize Shrink = c { frac = max 0 $ frac c - delta c }
          resize Expand = c { frac = min 1 $ frac c + delta c }
          incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

    description l = "HintedTile " ++ show (orientation l)

addBorder, substractBorder :: Dimension -> (Dimension, Dimension) -> (Dimension, Dimension)
addBorder       b (w, h) = (w + 2 * b, h + 2 * b)
substractBorder b (w, h) = (w - 2 * b, h - 2 * b)

getHints :: Window -> X SizeHints
getHints w = withDisplay $ \d -> io $ getWMNormalHints d w

-- Divide the screen vertically (horizontally) into n subrectangles
divideVertically, divideHorizontally :: Dimension -> [SizeHints] -> Rectangle -> [Rectangle]
divideVertically _ [] _ = [] -- there's a fold here, struggling to get out
divideVertically b (hints:rest) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideVertically b rest (Rectangle sx (sy + fromIntegral h) sw (sh - h)))
    where (w, h) = addBorder b $ applySizeHints hints $ substractBorder b
                   (sw, sh `div` fromIntegral (1 + (length rest)))

divideHorizontally _ [] _ = []
divideHorizontally b (hints:rest) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideHorizontally b rest (Rectangle (sx + fromIntegral w) sy (sw - w) sh))
    where (w, h) = addBorder b $ applySizeHints hints $ substractBorder b
                   (sw `div` fromIntegral (1 + (length rest)), sh)

-- Split the screen into two rectangles, using a rational to specify the ratio
splitHorizontally, splitVertically :: Rational -> Rectangle -> (Rectangle -> [Rectangle]) -> (Rectangle -> [Rectangle]) -> [Rectangle]
splitHorizontally f (Rectangle sx sy sw sh) left right = leftRects ++ rightRects
    where leftw = floor $ fromIntegral sw * f
          leftRects = left $ Rectangle sx sy leftw sh
          rightx = (maximum . map rect_width) leftRects
          rightRects = right $ Rectangle (sx + fromIntegral rightx) sy (sw - rightx) sh

splitVertically f (Rectangle sx sy sw sh) top bottom = topRects ++ bottomRects
    where toph = floor $ fromIntegral sh * f
          topRects = top $ Rectangle sx sy sw toph
          bottomy = (maximum . map rect_height) topRects
          bottomRects = bottom $ Rectangle sx (sy + fromIntegral bottomy) sw (sh - bottomy)
