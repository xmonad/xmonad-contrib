{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
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

module XMonad.Layout.TilePrime (
    -- * Usage
    -- $usage
    TilePrime(TilePrime)
    ) where

import Control.Monad (mplus)
import Control.Monad.Reader (asks)
import Data.List (mapAccumL)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWMNormalHints)
import XMonad.Operations
import XMonad.Layouts
import XMonad hiding (trace)
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TilePrime
--
-- Then edit your @layoutHook@ by adding the TilePrime layout:
--
-- > myLayouts = TilePrime 1 (3/100) (1/2) False ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- Use @True@ as the last argument to get a wide layout.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data TilePrime a = TilePrime
                         { nmaster     :: Int
                         , delta, frac :: Rational
                         , flipped     :: Bool
                         } deriving (Show, Read)

instance LayoutClass TilePrime Window where
  description c | flipped c = "TilePrime Horizontal"
                | otherwise = "TilePrime Vertical"

  pureMessage c m = fmap resize (fromMessage m) `mplus`
                    fmap incmastern (fromMessage m)
    where
    resize Shrink = c { frac = max 0 $ frac c - delta c }
    resize Expand = c { frac = min 1 $ frac c + delta c }
    incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

  doLayout TilePrime { frac = f, nmaster = m, flipped = flp } rect s = do
    bW <- asks (borderWidth . config)
    let xs = W.integrate s
    hints <- withDisplay $ \ disp -> io (mapM (getWMNormalHints disp) xs)
    let xs' = zip xs hints
        (leftXs, rightXs) = splitAt m xs'
        (leftRect, rightRect)
          | null rightXs = (rect, Rectangle 0 0 0 0)
          | null leftXs  = (Rectangle 0 0 0 0, rect)
          | flp          = splitVerticallyBy f rect
          | otherwise    = splitHorizontallyBy f rect
        masters = fillWindows bW leftRect leftXs
        slaves  = fillWindows bW rightRect rightXs
    return (masters ++ slaves, Nothing)

    where
    fillWindows bW r xs = snd $ mapAccumL (aux bW) (r,n) xs
      where n = fromIntegral (length xs) :: Rational

    aux bW (r,n) (x,hint) = ((rest,n-1),(x,r'))
      where
      (allocated, _) | flp       = splitHorizontallyBy (recip n) r
                     | otherwise = splitVerticallyBy (recip n) r

      (w,h) = underBorders bW (applySizeHints hint) (rect_D allocated)

      r'   = r { rect_width = w, rect_height = h }

      rest | flp       = r { rect_x      = rect_x r + toEnum (fromEnum w)
                           , rect_width  = rect_width r - w }
           | otherwise = r { rect_y      = rect_y r + toEnum (fromEnum h)
                           , rect_height = rect_height r - h }

rect_D :: Rectangle -> D
rect_D Rectangle { rect_width = w, rect_height = h } = (w,h)

-- | Transform a function on dimensions into one without regard for borders
underBorders :: Dimension -> (D -> D) -> D -> D
underBorders bW f = adjBorders bW 1 . f . adjBorders bW (-1)

-- | Modify dimensions by a multiple of the current borders
adjBorders                :: Dimension -> Dimension -> D -> D
adjBorders bW mult (w,h)  = (w+2*mult*bW, h+2*mult*bW)
