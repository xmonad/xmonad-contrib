{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.HintedTile
-- Description :  A gapless tiled layout that obeys window size hints.
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
    HintedTile(..), Orientation(..), Alignment(..)
) where

import XMonad hiding (Tall(..))
import qualified XMonad.StackSet as W
import XMonad.Prelude

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.HintedTile
--
-- Then edit your @layoutHook@ by adding the HintedTile layout:
--
-- > myLayout = hintedTile Tall ||| hintedTile Wide ||| Full ||| etc..
-- >   where
-- >      hintedTile = HintedTile nmaster delta ratio TopLeft
-- >      nmaster    = 1
-- >      ratio      = 1/2
-- >      delta      = 3/100
-- > main = xmonad def { layoutHook = myLayout }
--
-- Because both Xmonad and Xmonad.Layout.HintedTile define Tall,
-- you need to disambiguate Tall. If you are replacing the
-- built-in Tall with HintedTile, change @import Xmonad@ to
-- @import Xmonad hiding (Tall)@.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data HintedTile a = HintedTile
    { nmaster     :: !Int         -- ^ number of windows in the master pane
    , delta       :: !Rational    -- ^ how much to change when resizing
    , frac        :: !Rational    -- ^ ratio between master/nonmaster panes
    , alignment   :: !Alignment   -- ^ Where to place windows that are smaller
                                  --   than their preordained rectangles.
    , orientation :: !Orientation -- ^ Tall or Wide (mirrored) layout?
    } deriving ( Show, Read )

data Orientation
     = Wide -- ^ Lay out windows similarly to Mirror tiled.
     | Tall -- ^ Lay out windows similarly to tiled.
    deriving ( Show, Read, Eq, Ord )

data Alignment = TopLeft | Center | BottomRight
    deriving ( Show, Read, Eq, Ord )

instance LayoutClass HintedTile Window where
    doLayout HintedTile{ orientation = o, nmaster = nm, frac = f, alignment = al } r w' = do
        bhs <- mapM mkAdjust w
        let (masters, slaves) = splitAt nm bhs
        return (zip w (tiler masters slaves), Nothing)
     where
        w = W.integrate w'
        tiler masters slaves
            | null masters || null slaves = divide al o (masters ++ slaves) r
            | otherwise = split o f r (divide al o masters) (divide al o slaves)

    pureMessage c m = fmap resize     (fromMessage m) `mplus`
                      fmap incmastern (fromMessage m)
     where
        resize Shrink = c { frac = max 0 $ frac c - delta c }
        resize Expand = c { frac = min 1 $ frac c + delta c }
        incmastern (IncMasterN d) = c { nmaster = max 0 $ nmaster c + d }

    description l = show (orientation l)

align :: Alignment -> Position -> Dimension -> Dimension -> Position
align TopLeft     p _ _ = p
align Center      p a b = p + fromIntegral (a - b) `div` 2
align BottomRight p a b = p + fromIntegral (a - b)

-- Divide the screen vertically (horizontally) into n subrectangles
divide :: Alignment -> Orientation -> [D -> D] -> Rectangle -> [Rectangle]
divide _ _ [] _ = []
divide al _ [bh] (Rectangle sx sy sw sh) = [Rectangle (align al sx sw w) (align al sy sh h) w h]
    where
    (w, h) = bh (sw, sh)

divide al Tall (bh:bhs) (Rectangle sx sy sw sh) = Rectangle (align al sx sw w) sy w h :
      divide al Tall bhs (Rectangle sx (sy + fromIntegral h) sw (sh - h))
 where
    (w, h) = bh (sw, sh `div` fromIntegral (1 + length bhs))

divide al Wide (bh:bhs) (Rectangle sx sy sw sh) = Rectangle sx (align al sy sh h) w h :
      divide al Wide bhs (Rectangle (sx + fromIntegral w) sy (sw - w) sh)
 where
    (w, h) = bh (sw `div` fromIntegral (1 + length bhs), sh)

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
