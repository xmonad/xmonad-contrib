{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutHints
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- Make layouts respect size hints.
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutHints
    ( -- * usage
      -- $usage
      layoutHints
    , layoutHintsWithPlacement
    , LayoutHints
    , placeRectangle
    ) where

import XMonad hiding ( trace )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Decoration ( isInStack )

import Control.Applicative ( (<$>) )
import Control.Arrow ( second )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutHints
--
-- Then edit your @layoutHook@ by adding the 'layoutHints' layout modifier
-- to some layout:
--
-- > myLayouts = layoutHints (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- Or, to center the adapted window in its available area:
--
-- > myLayouts = layoutHintsWithPlacement (0.5, 0.5) (Tall 1 (3/100) (1/2))  
-- >                   ||| Full ||| etc..
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

layoutHints :: (LayoutClass l a) => l a -> ModifiedLayout LayoutHints l a
layoutHints = ModifiedLayout (LayoutHints (0, 0))

-- | @layoutHintsWithPlacement (rx, ry) layout@ will adapt the sizes of a layout's
-- windows according to their size hints, and position them inside their
-- originally assigned area according to the @rx@ and @ry@ parameters.
-- (0, 0) places the window at the top left, (1, 0) at the top right, (0.5, 0.5)
-- at the center, etc.
layoutHintsWithPlacement :: (LayoutClass l a) => (Double, Double) 
                         -> l a -> ModifiedLayout LayoutHints l a
layoutHintsWithPlacement rs = ModifiedLayout (LayoutHints rs)

data LayoutHints a = LayoutHints (Double, Double) 
                     deriving (Read, Show)

instance LayoutModifier LayoutHints Window where
    modifierDescription _ = "Hinted"
    redoLayout _ _ Nothing  xs = return (xs, Nothing)
    redoLayout (LayoutHints al) _ (Just s) xs 
        = do xs' <- mapM (\x@(_, r) -> second (placeRectangle al r) <$> applyHint x) xs
             return (xs', Nothing)
     where
        applyHint (w,r@(Rectangle a b c d)) = do
            adj <- mkAdjust w
            let (c',d') = adj (c,d)
            return (w, if isInStack s w then Rectangle a b c' d' else r)

-- | @placeRectangle (rx, ry) r0 r@ will return a new rectangle with the same dimensions
-- as @r@, but positioned inside of @r0@ as specified by the (rx, ry) parameters (see 
-- 'layoutHintsWithPlacement').
placeRectangle :: RealFrac r => (r, r) -> Rectangle -> Rectangle -> Rectangle
placeRectangle (rx, ry) (Rectangle x0 y0 w h) (Rectangle _ _ dx dy)
    = Rectangle (align x0 dx w rx) (align y0 dy h ry) dx dy
    where align :: RealFrac r => Position -> Dimension -> Dimension -> r -> Position
          align z0 dz d r = z0 + truncate (fromIntegral (d - dz) * r)