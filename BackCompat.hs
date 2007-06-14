-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.BackCompat
-- Copyright   :  (c) daniel@wagner-home.com
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  daniel@wagner-home.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that provides back compatibility with GHC 6.4
--
-----------------------------------------------------------------------------
module XMonadContrib.BackCompat (
                                 -- * Usage
                                 -- $usage
                                 forM, forM_
                                ) where

import Data.Map (Map, fromList)
import GHC.Read

{- $usage
 
This file will contain all the things GHC 6.4 users need to compile xmonad.
Currently, the steps to get compilation are:
add the following line to StackSet.hs, Operations.hs, and Main.hs:

> import XMonadContrib.BackCompat

-}

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

-- not used yet, but just in case
forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

instance (Ord k, Read k, Read e) => Read (Map k e) where
  readsPrec _ = \s1 -> do
    ("{", s2) <- lex s1
    (xs,  s3) <- readPairs s2
    ("}", s4) <- lex s3
    return (fromList xs, s4)

-- parses a pair of things with the syntax a:=b
-- stolen from the GHC 6.6 sources
readPair :: (Read a, Read b) => ReadS (a,b)
readPair s = do (a, ct1)    <- reads s
                (":=", ct2) <- lex ct1
                (b, ct3)    <- reads ct2
                return ((a,b), ct3)

readPairs :: (Read a, Read b) => ReadS [(a,b)]
readPairs s1 = case readPair s1 of
    [(p, s2)]   -> case s2 of
                        (',':s3)    -> do
                            (ps, s4) <- readPairs s3
                            return (p:ps, s4)
                        _           -> [([p], s2)]
    _           -> [([],s1)]
