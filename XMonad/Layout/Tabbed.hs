{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Tabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  droundy@darcs.net, andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module XMonad.Layout.Tabbed
    ( -- * Usage:
      -- $usage
      tabbed
    , Theme (..)
    , defaultTheme
    , TabbedDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest ( Simplest(Simplest) )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Tabbed
--
-- Then edit your @layoutHook@ by adding the Tabbed layout:
--
-- > myLayouts = tabDeco shrinkText defaultTheme ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > myTabConfig = defaultTheme { inactiveBorderColor = "#FF0000"
-- >                                   , activeTextColor = "#00FF00"}
--
-- and
--
-- > mylayout = tabDeco shrinkText myTabConfig ||| Full ||| etc..

-- | This function is deprecated and will be removed before 0.7!!
tabbed :: (Eq a, Shrinker s) => s -> Theme
       -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbed s c = decoration s c Tabbed Simplest

data TabbedDecoration a = Tabbed deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco  _ = "Tabbed"
    decorateFirst _ = False
    pureDecoration _ _ ht (Rectangle x y wh _) s wrs (w,_) = Just $ Rectangle nx y nwh (fi ht)
        where nwh = wh `div` max 1 (fi $ length wrs)
              nx  = case w `elemIndex` (S.integrate s) of
                      Just i  -> x + (fi nwh * fi i)
                      Nothing -> x
