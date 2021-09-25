{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DwmStyle
-- Description :  A layout modifier for decorating windows in a dwm like style.
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
    , Theme (..)
    , def
    , DwmStyle (..)
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
-- > myL = dwmStyle shrinkText def (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > myDWConfig = def { inactiveBorderColor = "red"
-- >                  , inactiveTextColor   = "red"}
--
-- and
--
-- > myL = dwmStyle shrinkText myDWConfig (layoutHook def)
--
-- A complete xmonad.hs file for this would therefore be:
--
-- > import XMonad
-- > import XMonad.Layout.DwmStyle
-- >
-- > main = xmonad def {
-- >     layoutHook = dwmStyle shrinkText def (layoutHook def)
-- >     }
--


-- | Add simple old dwm-style decorations to windows of a layout.
dwmStyle :: (Eq a, Shrinker s) => s -> Theme
         -> l a -> ModifiedLayout (Decoration DwmStyle s) l a
dwmStyle s c = decoration s c Dwm

data DwmStyle a = Dwm deriving (Show, Read)

instance Eq a => DecorationStyle DwmStyle a where
    describeDeco _ = "DwmStyle"
    shrink  _ _  r = r
    pureDecoration _ wh ht _ s@(Stack fw _ _) _ (w,Rectangle x y wid _) =
        if w == fw || not (isInStack s w) then Nothing else Just $ Rectangle (fi nx) y nwh (fi ht)
            where nwh = min wid $ fi wh
                  nx  = fi x + wid - nwh
