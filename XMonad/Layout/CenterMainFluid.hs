{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CenterMainFluid
-- Description :  Three column layout with master in center and unoccupied spaces reserved.
-- Copyright   :  (c) 2023 Mahdi Seyedan
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Mahdi Seyedan. <mahdisn78@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A three column layout with main column in the center and
-- two stack columns surrounding it. There will be always
-- a pane in the center column and unoccupied spaces on the
-- sides are reserved.
-- It's best suited for ultrawide montiors, where a single
-- stretched window might be annoying.
-----------------------------------------------------------------------------

module XMonad.Layout.CenterMainFluid
  ( -- * Usage
    -- $usage
    CenterMainFluid (..)
  ) where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad (msum)

-- $usage
-- You can use this module by adding following in your @xmonad.hs@:
--
-- > import XMonad.Layout.CenterMainFluid
--
-- Then edit your @layoutHook@ by adding the CenterMainFluid layout:
--
-- > myLayoutHook = CenterMainFluid 1 (3/100) (70/100) ||| ...
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the center
-- column. The second argument specifies the amount to resize while resizing
-- and the third argument specifies the initial size of the center column.
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".


-- | Arguments are nmaster, delta, fraction. Supports 'Shrink', 'Expand' and
-- 'IncMasterN'
data CenterMainFluid a = CenterMainFluid
  { cmfNMaster :: !Int             -- ^ The default number of windows in the center pane (default: 1)
  , cmfRatioIncrement :: !Rational -- ^ Percent of screen to increment by when resizing panes (default: 3/100)
  , cmfRatio :: !Rational          -- ^ Default proportion of screen occupied by the center pane (default: 70/100)
  }
  deriving (Show,Read)

instance LayoutClass CenterMainFluid a where

    pureLayout (CenterMainFluid nmaster _ frac) r s
        | frac == 0 = drop nmaster layout
        | frac == 1 = take nmaster layout
        | otherwise = layout
      where layout = zip ws rs
            ws = W.integrate s
            rs = tile3 frac r nmaster (length ws)

    pureMessage (CenterMainFluid nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = CenterMainFluid nmaster delta (max 0 $ frac-delta)
            resize Expand             = CenterMainFluid nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = CenterMainFluid (max 0 (nmaster+d)) delta frac

    description _ = "CenterMainFluid"

tile3 :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 f r nmaster n
  | nmaster <= 0 || n <= nmaster = splitVertically n middleR
  | otherwise = masters ++ rights ++ lefts
      where (leftR, middleR, rightR) = split3HorizontallyBy f r
            (halfN, remaining) = (n - nmaster) `divMod` 2
            masters = splitVertically nmaster middleR
            lefts = splitVertically halfN leftR
            rights = splitVertically (halfN + remaining) rightR

-- | Divide the screen into three rectangles, using a rational to specify the ratio of center one
split3HorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
  ( Rectangle sx sy sidew sh
  , Rectangle (sx + fromIntegral sidew) sy middlew sh
  , Rectangle (sx + fromIntegral sidew + fromIntegral middlew) sy sidew sh
  )
  where middlew = floor $ fromIntegral sw * f
        sidew = (sw - fromIntegral middlew) `div` 2
