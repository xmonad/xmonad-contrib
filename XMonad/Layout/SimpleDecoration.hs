{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SimpleDecoration
-- Description :  Add simple decorations to the windows of a given layout.
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier for adding simple decorations to the windows of a
-- given layout. The decorations are in the form of ion-like tabs
-- for window titles.
--
-----------------------------------------------------------------------------

module XMonad.Layout.SimpleDecoration
    ( -- * Usage:
      -- $usage
      simpleDeco
    , Theme (..)
    , def
    , SimpleDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import XMonad
import XMonad.Layout.Decoration

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.SimpleDecoration
--
-- Then edit your @layoutHook@ by adding the SimpleDecoration decoration to
-- your layout:
--
-- > myL = simpleDeco shrinkText def (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > mySDConfig = def { inactiveBorderColor = "red"
-- >                  , inactiveTextColor   = "red"}
--
-- and
--
-- > myL = simpleDeco shrinkText mySDConfig (layoutHook def)

-- | Add simple decorations to windows of a layout.
simpleDeco :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration SimpleDecoration s) l a
simpleDeco s c = decoration s c $ Simple True

newtype SimpleDecoration a = Simple Bool deriving (Show, Read)

instance Eq a => DecorationStyle SimpleDecoration a where
    describeDeco _ = "Simple"
    shrink (Simple b) (Rectangle _ _ _ dh) r@(Rectangle x y w h) =
        if b then Rectangle x (y + fi dh) w (h - dh) else r
    pureDecoration (Simple b) wh ht _ s _ (w,Rectangle x y wid _) =
        if isInStack s w
        then if b
             then Just $ Rectangle x  y          nwh ht
             else Just $ Rectangle x (y - fi ht) nwh ht
        else Nothing
            where nwh = min wid wh
