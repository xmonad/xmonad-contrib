{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.LayoutCombinators
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for combining Layouts
-----------------------------------------------------------------------------

module XMonadContrib.LayoutCombinators (
    -- * Usage
    -- $usage
    (<|>), (</>), (<||>), (<//>)
    ) where

import XMonad
import Operations ( Tall(..), Mirror(..) )
import XMonadContrib.Combo
import XMonadContrib.DragPane

-- $usage
-- Use LayoutCombinators to easily combine Layouts.

(<||>), (<//>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
                  l1 a -> l2 a -> CombineTwo DragPane l1 l2 a
(<|>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
         => l1 a -> l2 a -> CombineTwo Tall l1 l2 a
(</>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
         => l1 a -> l2 a -> CombineTwo (Mirror Tall) l1 l2 a

(<||>) = combineTwo (dragPane Vertical 0.1 0.5)
(<//>) = combineTwo (dragPane Horizontal 0.1 0.5)
(<|>) = combineTwo (Tall 1 0.1 0.5)
(</>) = combineTwo (Mirror $ Tall 1 0.1 0.5)
