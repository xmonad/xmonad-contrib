{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Named
-- Description :  Assign a name to a given layout.
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for assigning a name to a given layout. Deprecated, use
-- "XMonad.Layout.Renamed" instead.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Named
    ( -- * Usage
      -- $usage
      named,
      nameTail
    ) where

import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Named
--
-- Then edit your @layoutHook@ by adding the Named layout modifier
-- to some layout:
--
-- > myLayout = named "real big" Full ||| (nameTail $ named "real big" $ Full) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- Note that this module has been deprecated and may be removed in a future
-- release, please use "XMonad.Layout.Renamed" instead.

-- | (Deprecated) Rename a layout.
named :: String -> l a -> ModifiedLayout Rename l a
named s = renamed [Replace s]

-- | (Deprecated) Remove the first word of the name.
nameTail :: l a -> ModifiedLayout Rename l a
nameTail = renamed [CutWordsLeft 1]
