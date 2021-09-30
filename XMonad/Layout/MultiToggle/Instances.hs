{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiToggle.Instances
-- Description :  Common instances for "XMonad.Layout.MultiToggle".
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
import XMonad.Layout.LayoutModifier

data StdTransformers = FULL          -- ^ switch to Full layout
                     | NBFULL        -- ^ switch to Full with no borders
                     | MIRROR        -- ^ Mirror the current layout.
                     | NOBORDERS     -- ^ Remove borders.
                     | SMARTBORDERS  -- ^ Apply smart borders.
  deriving (Read, Show, Eq)

instance Transformer StdTransformers Window where
    transform FULL         x k = k Full (const x)
    transform NBFULL       x k = k (noBorders Full) (const x)
    transform MIRROR       x k = k (Mirror x) (\(Mirror x') -> x')
    transform NOBORDERS    x k = k (noBorders x) (\(ModifiedLayout _ x') -> x')
    transform SMARTBORDERS x k = k (smartBorders x) (\(ModifiedLayout _ x') -> x')
