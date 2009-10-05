{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Types
-- Copyright   :  (c) Daniel Schoepe (2009)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Miscellaneous commonly used types.
--
-----------------------------------------------------------------------------

module XMonad.Util.Types (Direction1D(..)
                         ,Direction2D(..)
                         ) where

import Data.Typeable (Typeable)

-- | One-dimensional directions:
data Direction1D = Next | Prev deriving (Eq,Read,Show,Typeable)

-- | Two-dimensional directions:
data Direction2D = U -- ^ Up
                 | D -- ^ Down
                 | R -- ^ Right
                 | L -- ^ Left
                   deriving (Eq,Read,Show,Ord,Enum,Bounded,Typeable)
