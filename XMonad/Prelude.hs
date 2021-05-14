{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prelude
-- Copyright   :  slotThe <soliditsallgood@mailbox.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  slotThe <soliditsallgood@mailbox.org>
--
-- Utility functions and re-exports for a more ergonomic developing
-- experience.  Users themselves will not find much use here.
--
--------------------------------------------------------------------
module XMonad.Prelude (
    module Exports,
    fi,
    chunksOf,
    (.:),
    (!?),
) where

import Control.Applicative as Exports
import Control.Monad       as Exports
import Data.Bool           as Exports
import Data.Char           as Exports
import Data.Foldable       as Exports
import Data.Function       as Exports
import Data.Functor        as Exports
import Data.List           as Exports
import Data.Maybe          as Exports
import Data.Monoid         as Exports
import Data.Traversable    as Exports

-- | Short for 'fromIntegral'.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Given a maximum length, splits a list into sublists
--
-- >>> chunksOf 5 (take 30 $ repeat 'a')
-- ["aaaaa","aaaaa","aaaaa","aaaaa","aaaaa","aaaaa"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = chunk : chunksOf i rest
  where !(chunk, rest) = splitAt i xs

-- | Safe version of '(!!)'.
(!?) :: [a] -> Int -> Maybe a
(!?) xs n = listToMaybe $ drop n xs

-- | Multivariant composition.
--
-- > f .: g ≡ (f .) . g ≡ \c d -> f (g c d)
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)
