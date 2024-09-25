{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CenteredIfSingle
-- Description :  If only a single window is shown, center it on screen
-- Copyright   :  (c) 2021 Leon Kowarschick
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <TheElkOfWar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that, if there is only a single window on screen, places
-- that window in the center of the screen.
-- This is especially useful on wide screen setups, where the window would otherwise
-- be unnecessarily far away from the center of your field of vision.
--
-----------------------------------------------------------------------------

module XMonad.Layout.CenteredIfSingle
  ( -- * Usage
    -- $usage
    centeredIfSingle, CenteredIfSingle
  ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (fi)

-- $usage
-- You can use this module by including  the following in your @xmonad.hs@:
--
-- > import XMonad.Layout.CenteredIfSingle
--
-- and adding the 'centeredIfSingle' layoutmodifier to your layouts.
--
-- > myLayoutHook = centeredIfSingle 0.7 0.8 Grid ||| ...
--
-- For more information on configuring your layouts see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>
-- and "XMonad.Doc.Extending".


-- | Layout Modifier that places a window in the center of the screen,
-- leaving room on the left and right if there is only a single window.
-- The first argument is the horizontal and the second one the vertical
-- ratio of the screen the centered window should take up.  Both numbers
-- should be between 0.0 and 1.0.
data CenteredIfSingle a = CenteredIfSingle !Double !Double
  deriving (Show, Read)

instance LayoutModifier CenteredIfSingle Window where
  pureModifier (CenteredIfSingle ratioX ratioY) r _ [(onlyWindow, _)] = ([(onlyWindow, rectangleCenterPiece ratioX ratioY r)], Nothing)
  pureModifier _ _ _ winRects = (winRects, Nothing)

-- | Layout Modifier that places a window in the center of the screen,
-- leaving room on all sides if there is only a single window
centeredIfSingle :: Double -- ^ Horizontal ratio of the screen the centered window should take up; should be a value between 0.0 and 1.0
                 -> Double -- ^ Vertical ratio; should also be a value between 0.0 and 1.0
                 -> l a    -- ^ The layout that will be used if more than one window is open
                 -> ModifiedLayout CenteredIfSingle l a
centeredIfSingle ratioX ratioY = ModifiedLayout (CenteredIfSingle ratioX ratioY)

-- | Calculate the center piece of a rectangle given the percentage of the outer rectangle it should occupy.
rectangleCenterPiece :: Double -> Double -> Rectangle -> Rectangle
rectangleCenterPiece ratioX ratioY (Rectangle rx ry rw rh) = Rectangle startX startY width height
  where
    startX = rx + left
    startY = ry + top

    width  = newSize rw left
    height = newSize rh top

    left = rw `scaleBy` ratioX
    top  = rh `scaleBy` ratioY

newSize :: Dimension -> Position -> Dimension
newSize dim pos = fi $ fi dim - pos * 2

scaleBy :: Dimension -> Double -> Position
scaleBy dim ratio = floor $ fi dim * (1.0 - ratio) / 2
