-- --------------------------------------------------------------------------
-- -- |
-- -- Module      :  TilePrime.hs
-- -- Copyright   :  (c) Eric Mertens 2007
-- -- License     :  BSD3-style (see LICENSE)
-- --
-- -- Maintainer  :  emertens@gmail.com
-- -- Stability   :  unstable
-- -- Portability :  not portable
-- --
-- -- TilePrime. Tile windows filling gaps created by resize hints
-- --
-- -----------------------------------------------------------------------------
--

module XMonadContrib.TilePrime (TilePrime(TilePrime)) where

import Control.Monad (mplus)
import Data.List (genericLength)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWMNormalHints)
import Operations
import XMonad hiding (trace)
import qualified StackSet as W
import {-#SOURCE#-} Config (borderWidth)

data TilePrime a = TilePrime
                         { nmaster     :: Int
                         , delta, frac :: Rational
                         , flipped     :: Bool
                         } deriving (Show, Read)

instance LayoutClass TilePrime Window where
  description _ = "TilePrime"

  pureMessage c m = fmap resize (fromMessage m) `mplus`
                    fmap incmastern (fromMessage m)
    where
    resize Shrink = c { frac = max 0 $ frac c - delta c }
    resize Expand = c { frac = min 1 $ frac c + delta c }
    incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

  doLayout c rect s = do
    let flp = flipped c
    let xs = W.integrate s
    hints <- withDisplay $ \ disp -> io (mapM (getWMNormalHints disp) xs)
    let xs' = zip xs hints
        (leftRect, rightRect)
          | flp       = splitVerticallyBy (frac c) rect
          | otherwise = splitHorizontallyBy (frac c) rect
        masters = fillWindows flp leftRect (take (nmaster c) xs')
        slaves  = fillWindows flp rightRect (drop (nmaster c) xs')
    return (masters ++ slaves, Nothing)

    where

    fillWindows _ _ [] = []
    fillWindows flp r ((x,hint):xs) = (x,r') : fillWindows flp rest xs
      where
      n = 1 + genericLength xs :: Rational

      (alloca, _) | flp       = splitHorizontallyBy (recip n) r
                  | otherwise = splitVerticallyBy (recip n) r

      (w,h) = applySizeHints hint `underBorders` (rect_width alloca, rect_height alloca)

      r'   = r { rect_width = w, rect_height = h }

      rest | flp       = r { rect_x      = rect_x r + toEnum (fromEnum w)
                           , rect_width  = rect_width r - w }
           | otherwise = r { rect_y      = rect_y r + toEnum (fromEnum h)
                           , rect_height = rect_height r - h }

-- | Transform a function on dimensions into one without regard for borders
underBorders :: (D -> D) -> D -> D
underBorders f = adjBorders 1 . f . adjBorders (-1)

-- | Modify dimensions by a multiple of the current borders
adjBorders             :: Dimension -> D -> D
adjBorders mult (w,h)  = (w+2*mult*borderWidth, h+2*mult*borderWidth)
