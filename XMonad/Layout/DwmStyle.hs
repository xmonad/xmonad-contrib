{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DwmStyle
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier for decorating windows in a dwm like style.
-----------------------------------------------------------------------------

module XMonad.Layout.DwmStyle
    ( -- * Usage:
      -- $usage
      dwmStyle
    , DeConfig (..)
    , DwmStyle (..), defaultDwmStyleConfig
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import XMonad
import XMonad.StackSet ( Stack (..) )
import XMonad.Layout.Decoration

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DwmStyle
--
-- Then edit your @layoutHook@ by adding the DwmStyle decoration to
-- your layout:
--
-- > myL = dwmStyle shrinkText defaultDwmStyleConfig (layoutHook defaultConfig)
-- > main = xmonad defaultConfig { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > myDWConfig = defaultDwmStyleConfig { inactiveBorderColor = "red"
-- >                                    , inactiveTextColor   = "red"}
--
-- and
--
-- > myL = dwmStyle shrinkText myDWConfig (layoutHook defaultConfig)

-- | Add simple old dwm-style decorations to windows of a layout.
dwmStyle :: (Eq a, Shrinker s) => s -> DeConfig DwmStyle a
         -> l a -> ModifiedLayout (Decoration DwmStyle s) l a
dwmStyle s c = decoration s c

defaultDwmStyleConfig :: Eq a => DeConfig DwmStyle a
defaultDwmStyleConfig= mkDefaultDeConfig Dwm

data DwmStyle a = Dwm deriving (Show, Read)

instance Eq a => DecorationStyle DwmStyle a where
    describeDeco _ = "DwmStyle"
    shrink  _ _  r = r
    pureDecoration _ wh ht _ (Stack fw _ _) _ (win,Rectangle x y wid _) =
        if win == fw then Nothing else Just $ Rectangle (fi nx) y nwh (fi ht)
            where nwh = min wid $ fi wh
                  nx  = fi x + wid - nwh
