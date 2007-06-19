-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.HintedTile
-- Copyright   :  (c) Peter De Wachter <pdewacht@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A gapless tiled layout that attempts to obey window size hints, 
-- rather than simply ignoring them.
--
-----------------------------------------------------------------------------

module XMonadContrib.HintedTile (
                                 -- * Usage
                                 -- $usage
                                 tall, wide) where

import XMonad
import Operations (Resize(..), IncMasterN(..), applySizeHints)
import qualified StackSet as W
import {-# SOURCE #-} Config (borderWidth)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Control.Monad

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.HintedTile

-- this sucks
addBorder, substractBorder :: (Dimension, Dimension) -> (Dimension, Dimension)
addBorder (w, h) = (w + 2 * borderWidth, h + 2 * borderWidth)
substractBorder (w, h) = (w - 2 * borderWidth, h - 2 * borderWidth)


tall, wide :: Int -> Rational -> Rational -> Layout Window
wide = tile splitVertically divideHorizontally
tall = tile splitHorizontally divideVertically

tile split divide nmaster delta frac =
    Layout { doLayout     = \r w' -> let w = W.integrate w'
                                     in do { hints <- sequence (map getHints w)
                                           ; return $ zip w (tiler frac r `uncurry` splitAt nmaster hints) }
           , modifyLayout = \m -> return $ fmap resize     (fromMessage m) `mplus`
                                           fmap incmastern (fromMessage m) }

    where resize Shrink = tile split divide nmaster delta (frac-delta)
          resize Expand = tile split divide nmaster delta (frac+delta)
          incmastern (IncMasterN d) = tile split divide (max 0 (nmaster+d)) delta frac

          tiler f r masters slaves = if null masters || null slaves
              then divide (masters ++ slaves) r
              else split f r (divide masters) (divide slaves)

getHints :: Window -> X SizeHints
getHints w = withDisplay $ \d -> io $ getWMNormalHints d w

--
-- Divide the screen vertically (horizontally) into n subrectangles
--
divideVertically, divideHorizontally :: [SizeHints] -> Rectangle -> [Rectangle]
divideVertically [] _ = [] -- there's a fold here, struggling to get out
divideVertically (hints:rest) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideVertically rest (Rectangle sx (sy + fromIntegral h) sw (sh - h)))
    where (w, h) = addBorder $ applySizeHints hints $ substractBorder
                   (sw, sh `div` fromIntegral (1 + (length rest)))

divideHorizontally [] _ = []
divideHorizontally (hints:rest) (Rectangle sx sy sw sh) = (Rectangle sx sy w h) :
      (divideHorizontally rest (Rectangle (sx + fromIntegral w) sy (sw - w) sh))
    where (w, h) = addBorder $ applySizeHints hints $ substractBorder
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
