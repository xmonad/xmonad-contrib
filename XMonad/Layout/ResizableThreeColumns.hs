{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ResizableThreeColumns
-- Description :  Like "XMonad.Layout.ThreeColumns", but allows resizing.
-- Copyright   :  (c) Sam Tay <sam.chong.tay@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  ?
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout similar to tall but with three columns. With 2560x1600 pixels this
-- layout can be used for a huge main window and up to six reasonable sized
-- resizable slave windows.
-----------------------------------------------------------------------------

module XMonad.Layout.ResizableThreeColumns (
                              -- * Usage
                              -- $usage
                              ResizableThreeCol(..), MirrorResize(..)
                             ) where

import XMonad hiding (splitVertically)
import XMonad.Prelude
import XMonad.Layout.ResizableTile(MirrorResize(..))
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Ratio

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ResizableThreeColumns
--
-- Then edit your @layoutHook@ by adding the ResizableThreeCol layout:
--
-- > myLayout = ResizableThreeCol 1 (3/100) (1/2) [] ||| ResizableThreeColMid 1 (3/100) (1/2) [] ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- The first argument specifies how many windows initially appear in the main
-- window. The second argument argument specifies the amount to resize while
-- resizing and the third argument specifies the initial size of the columns.
-- A positive size designates the fraction of the screen that the main window
-- should occupy, but if the size is negative the absolute value designates the
-- fraction a slave column should occupy. If both slave columns are visible,
-- they always occupy the same amount of space.
--
-- You may also want to add the following key bindings:
--
-- > , ((modm,               xK_a), sendMessage MirrorShrink)
-- > , ((modm,               xK_z), sendMessage MirrorExpand)
--
-- The ResizableThreeColMid variant places the main window between the slave columns.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- | Arguments are nmaster, delta, fraction
data ResizableThreeCol a
  = ResizableThreeColMid
    { threeColNMaster :: !Int
    , threeColDelta :: !Rational
    , threeColFrac :: !Rational
    , threeColSlaves :: [Rational]
    }
  | ResizableThreeCol
    { threeColNMaster :: !Int
    , threeColDelta :: !Rational
    , threeColFrac :: !Rational
    , threeColSlaves :: [Rational]
    } deriving (Show,Read)

instance LayoutClass ResizableThreeCol a where
  doLayout (ResizableThreeCol n _ f mf) r    = doL False n f mf r
  doLayout (ResizableThreeColMid n _ f mf) r = doL True  n f mf r
  handleMessage l m = do
    ms <- W.stack . W.workspace . W.current <$> gets windowset
    fs <- M.keys . W.floating <$> gets windowset
    return $ do
      s <- ms
      -- make sure current stack isn't floating
      guard (W.focus s `notElem` fs)
      -- remove floating windows from stack
      let s' = s { W.up = W.up s \\ fs, W.down = W.down s \\ fs }
      -- handle messages
      msum [ fmap resize       (fromMessage m)
           , fmap (mresize s') (fromMessage m)
           , fmap incmastern   (fromMessage m)
           ]
    where
      resize Shrink = l { threeColFrac = max (-0.5) $ frac-delta }
      resize Expand = l { threeColFrac = min 1 $ frac+delta }
      mresize s MirrorShrink = mresize' s delta
      mresize s MirrorExpand = mresize' s (negate delta)
      mresize' s delt =
        let up = length $ W.up s
            total = up + length (W.down s) + 1
            pos = if up == (nmaster-1) || up == (total-1) then up-1 else up
            mfrac' = modifymfrac (mfrac ++ repeat 1) delt pos
        in l { threeColSlaves = take total mfrac'}
      modifymfrac [] _ _ = []
      modifymfrac (f:fx) d n
        | n == 0    = f+d : fx
        | otherwise = f : modifymfrac fx d (n-1)
      incmastern (IncMasterN x) = l { threeColNMaster = max 0 (nmaster+x) }
      nmaster = threeColNMaster l
      delta = threeColDelta l
      frac = threeColFrac l
      mfrac = threeColSlaves l
  description _ = "ResizableThreeCol"

doL :: Bool -> Int -> Rational -> [Rational] -> Rectangle
    -> W.Stack a -> X ([(a, Rectangle)], Maybe (layout a))
doL middle nmaster f mf r =
  return
  . (, Nothing)
  . ap zip (tile3 middle f (mf ++ repeat 1) r nmaster . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile3 middle f mf r nmaster n
  | n <= nmaster || nmaster == 0 = splitVertically mf n r
  | n <= nmaster+1 = splitVertically mf nmaster s1
                  ++ splitVertically (drop nmaster mf) (n-nmaster) s2
  | otherwise = concat [ splitVertically mf nmaster r1
                       , splitVertically (drop nmaster mf) nslave1 r2
                       , splitVertically (drop (nmaster + nslave1) mf) nslave2 r3
                       ]
  where
    (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
    (s1, s2)     = splitHorizontallyBy (if f<0 then 1+f else f) r
    nslave       = n - nmaster
    nslave1      = ceiling (nslave % 2)
    nslave2      = n - nmaster - nslave1

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r = [r]
splitVertically _ n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) =
  let smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f)
  in Rectangle sx sy sw smallh :
       splitVertically fx (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
  if middle
  then ( Rectangle (sx + fromIntegral r3w) sy r1w sh
       , Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh
       , Rectangle sx sy r3w sh )
  else ( Rectangle sx sy r1w sh
       , Rectangle (sx + fromIntegral r1w) sy r2w sh
       , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
  where
    r1w = ceiling $ fromIntegral sw * f
    r2w = ceiling $ (sw - r1w) % 2
    r3w = sw - r1w - r2w
