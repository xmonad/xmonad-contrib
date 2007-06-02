module XMonadContrib.ReadMap () where

{- An instance of Read for Data.Map.Map's; useful for people that are still
 - compiling under 6.4.  To use it, add the following line to StackSet.hs:
 - import XMonadContrib.ReadMap
 -}

import Data.Map (Map, fromList)
import GHC.Read

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
