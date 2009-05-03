{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Named
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for assigning a name to a given layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Named
    ( -- * Usage
      -- $usage
      named,
      nameTail
    ) where

import XMonad
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Named
--
-- Then edit your @layoutHook@ by adding the Named layout modifier
-- to some layout:
--
-- > myLayouts = named "real big" Full ||| (nameTail $ named "real big" $ Full) ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Rename a layout.
named :: String -> l a -> ModifiedLayout Named l a
named s = ModifiedLayout (Named s)

data Named a = Named String deriving ( Read, Show )

instance LayoutModifier Named a where
    modifyDescription (Named n) _ = n


-- | Remove the first word of the name.
nameTail :: l a -> ModifiedLayout NameTail l a
nameTail = ModifiedLayout NameTail

data NameTail a = NameTail deriving (Read,Show)

instance LayoutModifier NameTail a where
  modifyDescription NameTail i = dropWhile (==' ') $ dropWhile (/=' ') $ description i
