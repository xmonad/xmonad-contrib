{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CircleEx
-- Description :  An elliptical, overlapping layout—extended version.
-- Copyright   :  (c) Peter De Wachter, Ilya V. Portnov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Ilya V. Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Circle is an elliptical, overlapping layout. Original code by Peter De Wachter,
-- extended by Ilya Porntov.
-----------------------------------------------------------------------------

module XMonad.Layout.CircleEx (
    -- * Usage
    -- $usage
    CircleEx (..), circle, circleEx,
    CircleExMsg (..)
  )
  where

import Data.Ratio

import XMonad
import XMonad.StackSet (Stack)
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
--
-- The layout puts the first N windows (called master) into the center of
-- screen. All others (called secondary, or stack) are organized in a circle
-- (well, ellipse). When opening a new secondary window, its size will be
-- slightly smaller than that of its predecessor (this is configurable). If
-- the number of master windows is set to zero, all windows will be arranged
-- in a circle. If there is more than one master window, they will be stacked
-- in the center on top of each other. The size of each additional master
-- window will again be slightly smaller than that of the former.
--
-- Since a picture says more than a thousand words, you see one
-- <https://github.com/xmonad/xmonad-contrib/assets/50166980/90ef1273-5201-4380-8b94-9e62d3c98e1c here>.
--
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Layout.CircleEx
--
-- Then edit your @layoutHook@ by adding the 'CircleEx' layout:
--
-- > myCircle = circleEx {cDelta = -3*pi/4}
-- > myLayout = myCircle ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- This layout understands standard messages:
--
-- * 'IncMasterN': increase or decrease the number of master windows.
-- * 'Shrink' and 'Expand': change the size of master windows.
--
-- More layout-specific messages are also supported, see 'CircleExMsg' below.
--
-- For more detailed instructions on editing the layoutHook see:
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | The layout data type. It is recommended to not use the 'CircleEx' data
-- constructor directly, and instead rely on record update syntax; for
-- example: @circleEx {cMasterRatio = 4%5}@. In this way you can avoid nasty
-- surprises if one day additional fields are added to @CircleEx@.
data CircleEx a = CircleEx
    { cNMaster :: !Int          -- ^ Number of master windows. Default value is 1.
    , cMasterRatio :: !Rational -- ^ Size of master window in relation to screen size.
                                --   Default value is @4%5@.
    , cStackRatio :: !Rational  -- ^ Size of first secondary window in relation to screen size.
                                --   Default value is @3%5@.
    , cMultiplier :: !Rational  -- ^ Coefficient used to calculate the sizes of subsequent secondary
                                --   windows. The size of the next window is calculated as the
                                --   size of the previous one multiplied by this value.
                                --   This value is also used to scale master windows, in case
                                --   there is more than one.
                                --   Default value is @5%6@. Set this to 1 if you want all secondary
                                --   windows to have the same size.
    , cDelta :: !Double         -- ^ Angle of rotation of the whole circle layout. Usual values
                                --   are from 0 to 2π, although it will work outside
                                --   this range as well. Default value of 0 means that the first
                                --   secondary window will be placed at the right side of screen.
    } deriving (Eq, Show, Read)

-- | Circle layout with default settings:
--
-- * Number of master windows is set to 1
-- * @cMasterRatio@ is set to @70/99@, which is nearly @1/sqrt(2)@
-- * @cStackRatio@ is set to @2/5@
-- * @cMultiplier@ is set to 1, which means all secondary windows
--   will have the same size
--
-- This can be used as a drop-in replacement for "XMonad.Layout.Circle".
circle :: CircleEx a
circle = CircleEx 1 (70%99) (2%5) 1 0

-- | Another variant of default settings for circle layout:
--
-- * Number of master windows is set to 1
-- * @cMasterRatio@ is set to @4/5@
-- * @cStackRatio@ is set to @3/5@
-- * @cMultiplier@ is set to @5/6@
--
circleEx :: CircleEx a
circleEx = CircleEx 1 (4%5) (3%5) (5%6) 0

-- | Specific messages understood by CircleEx layout.
data CircleExMsg
  = Rotate !Double            -- ^ Rotate secondary windows by specific angle
  | IncStackRatio !Rational   -- ^ Increase (or decrease, with negative value) sizes of secondary windows
  | IncMultiplier !Rational   -- ^ Increase 'cMultiplier'.
  deriving (Eq, Show, Typeable)

instance Message CircleExMsg

instance LayoutClass CircleEx Window where
  doLayout :: CircleEx Window -> Rectangle -> Stack Window -> X ([(Window, Rectangle)], Maybe (CircleEx Window))
  doLayout layout rect stack = do
    result <- raiseFocus $ circleLayout layout rect $ W.integrate stack
    return (result, Nothing)

  pureMessage :: CircleEx Window -> SomeMessage -> Maybe (CircleEx Window)
  pureMessage layout m =
      msum [changeMasterN <$> fromMessage m,
            resize <$> fromMessage m,
            specific <$> fromMessage m]
    where
      deltaSize = 11 % 10

      resize :: Resize -> CircleEx a
      resize Shrink = layout {cMasterRatio = max 0.1 $ min 1.0 $ cMasterRatio layout / deltaSize}
      resize Expand = layout {cMasterRatio = max 0.1 $ min 1.0 $ cMasterRatio layout * deltaSize}

      changeMasterN :: IncMasterN -> CircleEx a
      changeMasterN (IncMasterN d) = layout {cNMaster = max 0 (cNMaster layout + d)}

      specific :: CircleExMsg -> CircleEx a
      specific (Rotate delta) = layout {cDelta = delta + cDelta layout}
      specific (IncStackRatio delta) = layout {cStackRatio = max 0.1 $ min 2.0 $ delta + cStackRatio layout}
      specific (IncMultiplier delta) = layout {cMultiplier = max 0.1 $ min 2.0 $ delta + cMultiplier layout}

circleLayout :: CircleEx a -> Rectangle -> [a] -> [(a, Rectangle)]
circleLayout _ _ [] = []
circleLayout (CircleEx {..}) rectangle wins =
    master (take cNMaster wins) ++ rest (drop cNMaster wins)
  where
    master :: [a] -> [(a, Rectangle)]
    master ws = zip ws $ map (placeCenter cMasterRatio cMultiplier rectangle)
                           [cNMaster-1, cNMaster-2 .. 0]
    rest :: [a] -> [(a, Rectangle)]
    rest ws = zip ws $ zipWith (placeSatellite cStackRatio cMultiplier rectangle)
                        (map (+ cDelta) [0, pi*2 / fromIntegral (length ws) ..])
                        [0 ..]


raiseFocus :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
raiseFocus wrs = do
  focused <- withWindowSet (return . W.peek)
  return $ case find ((== focused) . Just . fst) wrs of
             Just x  -> x : delete x wrs
             Nothing -> wrs

placeCenter :: Rational -> Rational -> Rectangle -> Int -> Rectangle
placeCenter ratio multiplier (Rectangle x y width height) n = Rectangle x' y' width' height'
  where
    m = ratio * multiplier ^ n
    width' = round (m * fromIntegral width)
    height' = round (m * fromIntegral height)
    x' = x + fromIntegral (width - width') `div` 2
    y' = y + fromIntegral (height - height') `div` 2

placeSatellite :: Rational -> Rational -> Rectangle -> Double -> Int -> Rectangle
placeSatellite ratio multiplier (Rectangle x y width height) alpha n =
    Rectangle x' y' width' height'
  where
    m = ratio * multiplier ^ n
    x' = x + round (rx + rx * cos alpha)
    y' = y + round (ry + ry * sin alpha)
    rx = fromIntegral (width - width') / 2
    ry = fromIntegral (height - height') / 2
    width' = round (fromIntegral width * m)
    height' = round (fromIntegral height * m)
