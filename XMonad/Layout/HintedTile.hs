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
import Control.Applicative ((<$>))
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.HintedTile
--
-- Then edit your @layoutHook@ by adding the HintedTile layout:
--
-- > myLayouts = HintedTile 1 0.1 0.5 Tall ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
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
    doLayout (HintedTile { orientation = o, nmaster = nm, frac = f }) r w' = do
        bhs <- mapM getHints w
        let (masters, slaves) = splitAt nm bhs
        return (zip w (tiler masters slaves), Nothing)
     where
        w = W.integrate w'
        tiler masters slaves
            | null masters || null slaves = divide o (masters ++ slaves) r
            | otherwise = split o f r (divide o masters) (divide o slaves)

    pureMessage c m = fmap resize     (fromMessage m) `mplus`
                      fmap incmastern (fromMessage m)
     where
        resize Shrink = c { frac = max 0 $ frac c - delta c }
        resize Expand = c { frac = min 1 $ frac c + delta c }
        incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

    description l = show (orientation l)

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
divide :: Orientation -> [(Dimension, SizeHints)] -> Rectangle -> [Rectangle]
divide _ [] _ = []
divide Tall (bh:bhs) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divide Tall bhs (Rectangle sx (sy + fromIntegral h) sw (sh - h)))
 where (w, h) = hintsUnderBorder bh (sw, sh `div` fromIntegral (1 + (length bhs)))

divide Wide (bh:bhs) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divide Wide bhs (Rectangle (sx + fromIntegral w) sy (sw - w) sh))
 where
    (w, h) = hintsUnderBorder bh (sw `div` fromIntegral (1 + (length bhs)), sh)

-- Split the screen into two rectangles, using a rational to specify the ratio
split :: Orientation -> Rational -> Rectangle -> (Rectangle -> [Rectangle])
      -> (Rectangle -> [Rectangle]) -> [Rectangle]
split Tall f (Rectangle sx sy sw sh) left right = leftRects ++ rightRects
 where
    leftw = floor $ fromIntegral sw * f
    leftRects = left $ Rectangle sx sy leftw sh
    rightx = (maximum . map rect_width) leftRects
    rightRects = right $ Rectangle (sx + fromIntegral rightx) sy (sw - rightx) sh

split Wide f (Rectangle sx sy sw sh) top bottom = topRects ++ bottomRects
 where
    toph = floor $ fromIntegral sh * f
    topRects = top $ Rectangle sx sy sw toph
    bottomy = (maximum . map rect_height) topRects
    bottomRects = bottom $ Rectangle sx (sy + fromIntegral bottomy) sw (sh - bottomy)
