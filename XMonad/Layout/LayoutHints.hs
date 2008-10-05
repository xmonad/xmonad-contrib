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
    , LayoutHints
    ) where

import XMonad hiding ( trace )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Decoration ( isInStack )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutHints
--
-- Then edit your @layoutHook@ by adding the LayoutHints layout modifier
-- to some layout:
--
-- > myLayouts = layoutHints (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

layoutHints :: (LayoutClass l a) => l a -> ModifiedLayout LayoutHints l a
layoutHints = ModifiedLayout LayoutHints

data LayoutHints a = LayoutHints deriving (Read, Show)

instance LayoutModifier LayoutHints Window where
    modifierDescription _ = "Hinted"
    redoLayout _ _ Nothing  xs = return (xs, Nothing)
    redoLayout _ _ (Just s) xs = do
                            xs' <- mapM applyHint xs
                            return (xs', Nothing)
     where
        applyHint (w,r@(Rectangle a b c d)) = do
            adj <- mkAdjust w
            let (c',d') = adj (c,d)
            return (w, if isInStack s w then Rectangle a b c' d' else r)
