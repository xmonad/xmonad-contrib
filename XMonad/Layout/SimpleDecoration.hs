{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SimpleDecoration
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier for adding simple decorations to the windows of a
-- given layout.
-----------------------------------------------------------------------------

module XMonad.Layout.SimpleDecoration
    ( -- * Usage:
      -- $usage
      simpleDeco
    , SimpleDecoration (..), defaultSimpleConfig
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
-- > myL = simpleDeco shrinkText defaultSimpleConfig (layoutHook defaultConfig)
-- > main = xmonad defaultConfig { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > mySDConfig = defaultSimpleConfig { inactiveBorderColor = "red"
-- >                                  , inactiveTextColor   = "red"}
--
-- and
--
-- > myL = dwmStyle shrinkText mySDConfig (layoutHook defaultConfig)

-- | Add simple decorations to windows of a layout.
simpleDeco :: Shrinker s => s -> DeConfig SimpleDecoration a
           -> l a -> ModifiedLayout (Decoration SimpleDecoration s) l a
simpleDeco s c = decoration s c

defaultSimpleConfig :: DeConfig SimpleDecoration a
defaultSimpleConfig = mkDefaultDeConfig $ Simple True

data SimpleDecoration a = Simple Bool deriving (Show, Read)

instance DecorationStyle SimpleDecoration a where
    describeDeco _ = "Simple"
    shrink (Simple b) (Rectangle _ _ _ dh) r@(Rectangle x y w h) =
        if b then Rectangle x (y + fi dh) w (h - dh) else r
    pureDecoration (Simple b) wh ht _ _ _ (_,Rectangle x y wid _) =
        if b then Just $ Rectangle x y nwh ht else Just $ Rectangle x (y - fi ht) nwh ht
            where nwh = min wid wh