{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module      :  XMonad.Layout.Cross
-- Description :  A Cross Layout with the main window in the center.
-- Copyright   :  (c) Luis Cabellos <zhen.sydow@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Luis Cabellos <zhen.sydow@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A Cross Layout with the main window in the center.
--
module XMonad.Layout.Cross(
                          -- * Usage
                          -- $usage
                          simpleCross
                          , Cross(..) ) where

import XMonad( Dimension, Rectangle(..), LayoutClass(..), Resize(..), fromMessage )
import XMonad.StackSet( focus, up, down )
import XMonad.Prelude( msum )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Cross
--
-- Then edit your @layoutHook@ by adding one of the Cross layouts:
--
-- > myLayout =  simpleCross ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--

-- apply a factor to a Rectangle Dimension
(<%>) :: Dimension -> Rational -> Dimension
d <%> f = floor $ f * fromIntegral d

-- | The Cross Layout draws the focused window in the center of the screen
--   and part of the other windows on the sides. The 'Shrink' and 'Expand'
--   messages increment the size of the main window.
--
--   The focus keybindings change the center window, while other windows
--   cycle through the side positions. With the Cross layout only four
--   windows are shown around the focused window, two ups and two downs,
--   no matter how many are in the current stack. I.e. focus down cycles the
--   window below focused into the center; focus up cycles the window above.
data Cross a = Cross {
      crossProp :: !Rational, -- ^ Proportion of screen occupied by the main window.
      crossInc  :: !Rational  -- ^ Percent of main window to increment by when resizing.
    }
    deriving( Show, Read )

-- | A simple Cross Layout. It places the focused window in the center.
--   The proportion of the screen used by the main window is 4\/5.
simpleCross :: Cross a
simpleCross = Cross (4/5) (1/100)

instance LayoutClass Cross a where
    pureLayout (Cross f _) r s = [(focus s, mainRect r f)]
                              ++ zip winCycle (upRects r f)
                              ++ zip (reverse winCycle) (downRects r f)
        where winCycle = up s ++ reverse (down s)

    pureMessage (Cross f d) m = msum [fmap resize (fromMessage m)]
        where resize Shrink = Cross (max (1/100) $ f - d) d
              resize Expand = Cross (min 1 $ f + d) d

    description _ = "Cross"

-- get the Rectangle for the focused window
mainRect :: Rectangle -> Rational -> Rectangle
mainRect (Rectangle rx ry rw rh) f = Rectangle
                                     (rx + fromIntegral (rw <%> invf))
                                     (ry + fromIntegral (rh <%> invf))
                                     (rw <%> f) (rh <%> f)
    where invf = (1/2) * (1-f)

-- get the rectangles for the up windows
upRects :: Rectangle -> Rational -> [Rectangle]
upRects r f = [topRectangle r nf, rightRectangle r nf]
    where nf = f * (8/10)

-- get the rectangles for the down windows
downRects :: Rectangle -> Rational -> [Rectangle]
downRects r f = [bottomRectangle r nf, leftRectangle r nf]
    where nf = f * (8/10)

topRectangle :: Rectangle -> Rational -> Rectangle
topRectangle (Rectangle rx ry rw rh) f = Rectangle
                                         (rx + fromIntegral (rw <%> ((1-f)*(1/2))))
                                         ry
                                         (rw <%> f) (rh <%> ((1-f)*(1/2)))

rightRectangle :: Rectangle -> Rational -> Rectangle
rightRectangle (Rectangle rx ry rw rh) f = Rectangle
                                           (rx + fromIntegral (rw - (rw <%> (1/2))))
                                           (ry + fromIntegral (rh <%> ((1-f)*(1/2))))
                                           (rw <%> (1/2)) (rh <%> f)

bottomRectangle :: Rectangle -> Rational -> Rectangle
bottomRectangle (Rectangle rx ry rw rh) f = Rectangle
                                            (rx + fromIntegral (rw <%> ((1-f)*(1/2))))
                                            (ry + fromIntegral (rh - (rh <%> ((1-f)*(1/2)))))
                                            (rw <%> f) (rh <%> ((1-f)*(1/2)))

leftRectangle :: Rectangle -> Rational -> Rectangle
leftRectangle (Rectangle rx ry rw rh) f = Rectangle
                                          rx
                                           (ry + fromIntegral (rh <%> ((1-f)*(1/2))))
                                           (rw <%> (1/2)) (rh <%> f)
