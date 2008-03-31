{-# OPTIONS_GHC -fglasgow-exts #-}
-- above is for compatibility with GHC 6.6.
{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiToggle.Instances
-- Copyright   :  (c) 2008  Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Some convenient common instances of the
-- 'XMonad.Layout.MultiToggle.Transformer' class, for use with
-- "XMonad.Layout.MultiToggle".

module XMonad.Layout.MultiToggle.Instances (
  StdTransformers(..)
) where

import XMonad.Layout.MultiToggle

import XMonad
import XMonad.Layout.NoBorders

data StdTransformers = FULL          -- ^ switch to Full layout
                     | NBFULL        -- ^ switch to Full with no borders
                     | MIRROR        -- ^ Mirror the current layout.
                     | NOBORDERS     -- ^ Remove borders.
                     | SMARTBORDERS  -- ^ Apply smart borders.
  deriving (Read, Show, Eq, Typeable)

instance Transformer StdTransformers Window where
    transform FULL         _ k = k Full
    transform NBFULL       _ k = k (noBorders Full)
    transform MIRROR       x k = k (Mirror x)
    transform NOBORDERS    x k = k (noBorders x)
    transform SMARTBORDERS x k = k (smartBorders x)
