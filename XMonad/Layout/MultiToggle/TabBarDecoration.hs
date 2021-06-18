{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiToggle.TabBarDecoration
-- Copyright   :  (c) 2018  Lucian Poston
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <lucianposton@pm.me>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a simple transformer for use with "XMonad.Layout.MultiToggle" to
-- dynamically toggle "XMonad.Layout.TabBarDecoration".
-----------------------------------------------------------------------------

module XMonad.Layout.MultiToggle.TabBarDecoration (
    SimpleTabBar(..)
) where

import XMonad.Layout.MultiToggle

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.TabBarDecoration

-- $usage
-- To use this module with "XMonad.Layout.MultiToggle", add the @SIMPLETABBAR@
-- to your layout For example, from a basic layout like
--
-- > layout = tiled ||| Full
--
-- Add @SIMPLETABBAR@ by changing it this to
--
-- > layout = mkToggle (single SIMPLETABBAR) (tiled ||| Full)
--
-- You can now dynamically toggle the 'XMonad.Layout.TabBarDecoration'
-- transformation by adding a key binding such as @mod-x@ as follows.
--
-- > ...
-- >   , ((modm,               xK_x     ), sendMessage $ Toggle SIMPLETABBAR)
-- > ...

-- | Transformer for "XMonad.Layout.TabBarDecoration".
data SimpleTabBar = SIMPLETABBAR deriving (Read, Show, Eq)
instance Transformer SimpleTabBar Window where
    transform _ x k = k (simpleTabBar x) (\(ModifiedLayout _ (ModifiedLayout _ x')) -> x')
