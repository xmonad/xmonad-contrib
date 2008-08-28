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
      named
    ) where

import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Named
--
-- Then edit your @layoutHook@ by adding the Named layout modifier
-- to some layout:
--
-- > myLayouts = named "real big" Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

named :: String -> l a -> ModifiedLayout Named l a
named s = ModifiedLayout (Named s)

data Named a = Named String deriving ( Read, Show )

instance LayoutModifier Named a where
    modifyDescription (Named n) _ = n
