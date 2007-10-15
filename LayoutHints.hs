{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.LayoutHints
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- Make layouts respect size hints.
-----------------------------------------------------------------------------

module XMonadContrib.LayoutHints (
    -- * usage
    -- $usage
    layoutHints,
    LayoutHints) where

import Operations ( applySizeHints, D )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( getWMNormalHints )
import {-#SOURCE#-} Config (borderWidth)
import XMonad hiding ( trace )
import XMonadContrib.LayoutModifier

-- $usage
-- > import XMonadContrib.LayoutHints
-- > layouts = [ layoutHints tiled , layoutHints $ Mirror tiled ]

-- %import XMonadContrib.LayoutHints
-- %layout , layoutHints $ tiled
-- %layout , layoutHints $ Mirror tiled

layoutHints :: (LayoutClass l a) => l a -> ModifiedLayout LayoutHints l a
layoutHints = ModifiedLayout LayoutHints

-- | Expand a size by the given multiple of the border width.  The
-- multiple is most commonly 1 or -1.
adjBorders             :: Dimension -> D -> D
adjBorders mult (w,h)  = (w+2*mult*borderWidth, h+2*mult*borderWidth)

data LayoutHints a = LayoutHints deriving (Read, Show)

instance LayoutModifier LayoutHints Window where
    modifierDescription _ = "Hinted"
    redoLayout _ _ _ xs = do
                            xs' <- mapM applyHint xs
                            return (xs', Nothing)
     where
        applyHint (w,Rectangle a b c d) =
            withDisplay $ \disp -> do
                sh <- io $ getWMNormalHints disp w
                let (c',d') = adjBorders 1 . applySizeHints sh . adjBorders (-1) $ (c,d)
                return (w, Rectangle a b c' d')
