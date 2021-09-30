{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.NoFrillsDecoration
-- Description :  Most basic version of decoration for windows.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Most basic version of decoration for windows without any additional
-- modifications. In contrast to "XMonad.Layout.SimpleDecoration" this will
-- result in title bars that span the entire window instead of being only the
-- length of the window title.
--
-----------------------------------------------------------------------------

module XMonad.Layout.NoFrillsDecoration
    ( -- * Usage:
      -- $usage
      noFrillsDeco

    , module XMonad.Layout.SimpleDecoration
    , NoFrillsDecoration
    ) where

import XMonad.Layout.Decoration
import XMonad.Layout.SimpleDecoration

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.NoFrillsDecoration
--
-- Then edit your @layoutHook@ by adding the NoFrillsDecoration to
-- your layout:
--
-- > myL = noFrillsDeco shrinkText def (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--

-- | Add very simple decorations to windows of a layout.
noFrillsDeco :: (Eq a, Shrinker s) => s -> Theme
             -> l a -> ModifiedLayout (Decoration NoFrillsDecoration s) l a
noFrillsDeco s c = decoration s c $ NFD True

newtype NoFrillsDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle NoFrillsDecoration a where
    describeDeco _ = "NoFrillsDeco"
