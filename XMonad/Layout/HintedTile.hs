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
                                 HintedTile(..), Orientation(..)) where

import XMonad
import XMonad.Layouts    (Resize(..), IncMasterN(..))
import XMonad.Operations (applySizeHints, D)
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Applicative ((<$>))
import Control.Monad.Reader

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.HintedTile
--
-- Then edit your @layoutHook@ by adding the HintedTile layout:
--
-- > myLayouts = HintedTile 1 0.1 0.5 Tall ||| Full ||| etc..
-- > main = xmonad dafaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data HintedTile a = HintedTile
    { nmaster     :: Int
    , delta, frac :: Rational
    , orientation :: Orientation
    } deriving ( Show, Read )

data Orientation = Wide | Tall deriving ( Show, Read )

instance LayoutClass HintedTile Window where
    doLayout c rect w' = do
        bhs <- mapM getHints w
        let (masters, slaves) = splitAt (nmaster c) bhs
        return (zip w (tiler (frac c) rect masters slaves), Nothing)
     where
        w = W.integrate w'
        (split, divide) = case orientation c of
                            Tall -> (splitHorizontally, divideVertically)
                            Wide -> (splitVertically,   divideHorizontally)
        tiler f r masters slaves
            | null masters || null slaves = divide (masters ++ slaves) r
            | otherwise = split f r (divide masters) (divide slaves)

    pureMessage c m = fmap resize (fromMessage m) `mplus`
                      fmap incmastern (fromMessage m)
     where
        resize Shrink = c { frac = max 0 $ frac c - delta c }
        resize Expand = c { frac = min 1 $ frac c + delta c }
        incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

    description l = "HintedTile " ++ show (orientation l)

adjBorder :: Dimension -> Dimension -> D -> D
adjBorder n b (w, h) = (w + n * 2 * b, h + n * 2 * b)

-- | Transform a function on dimensions into one without regard for borders
hintsUnderBorder :: (Dimension, SizeHints) -> D -> D
hintsUnderBorder (bW, h) = adjBorder bW 1 . applySizeHints h . adjBorder bW (-1)

getHints :: Window -> X (Dimension, SizeHints)
getHints w = withDisplay $ \d -> io $ liftM2 (,)
    (fromIntegral . wa_border_width <$> getWindowAttributes d w)
    (getWMNormalHints d w)

-- Divide the screen vertically (horizontally) into n subrectangles
divideVertically, divideHorizontally :: [(Dimension, SizeHints)] -> Rectangle -> [Rectangle]
divideVertically [] _ = [] -- there's a fold here, struggling to get out
divideVertically (bh:bhs) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideVertically bhs (Rectangle sx (sy + fromIntegral h) sw (sh - h)))
 where (w, h) = hintsUnderBorder bh (sw, sh `div` fromIntegral (1 + (length bhs)))

divideHorizontally [] _ = []
divideHorizontally (bh:bhs) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideHorizontally bhs (Rectangle (sx + fromIntegral w) sy (sw - w) sh))
 where
    (w, h) = hintsUnderBorder bh (sw `div` fromIntegral (1 + (length bhs)), sh)

-- Split the screen into two rectangles, using a rational to specify the ratio
splitHorizontally, splitVertically :: Rational -> Rectangle -> (Rectangle -> [Rectangle])
                                   -> (Rectangle -> [Rectangle]) -> [Rectangle]
splitHorizontally f (Rectangle sx sy sw sh) left right = leftRects ++ rightRects
 where
    leftw = floor $ fromIntegral sw * f
    leftRects = left $ Rectangle sx sy leftw sh
    rightx = (maximum . map rect_width) leftRects
    rightRects = right $ Rectangle (sx + fromIntegral rightx) sy (sw - rightx) sh

splitVertically f (Rectangle sx sy sw sh) top bottom = topRects ++ bottomRects
 where
    toph = floor $ fromIntegral sh * f
    topRects = top $ Rectangle sx sy sw toph
    bottomy = (maximum . map rect_height) topRects
    bottomRects = bottom $ Rectangle sx (sy + fromIntegral bottomy) sw (sh - bottomy)
