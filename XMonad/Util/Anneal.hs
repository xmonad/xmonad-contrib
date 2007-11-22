-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Anneal
-- Copyright   :  (c) David Roundy
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Requires the 'random' package
--
-----------------------------------------------------------------------------

module XMonad.Util.Anneal (-- * Usage
                           -- $usage
                           Rated(Rated), the_value, the_rating
                          , anneal, annealMax ) where

import System.Random ( StdGen, Random, mkStdGen, randomR )
import Control.Monad.State ( State, runState, put, get, gets, modify )

-- $usage
-- See "XMonad.Layout.Mosaic" for an usage example.

data Rated a b = Rated !a !b
                 deriving ( Show )
instance Functor (Rated a) where
    f `fmap` (Rated v a) = Rated v (f a)

the_value :: Rated a b -> b
the_value (Rated _ b) = b
the_rating :: Rated a b -> a
the_rating (Rated a _) = a

instance Eq a => Eq (Rated a b) where
    (Rated a _) == (Rated a' _) = a == a'
instance Ord a => Ord (Rated a b) where
    compare (Rated a _) (Rated a' _) = compare a a'

anneal :: a -> (a -> Double) -> (a -> [a]) -> Rated Double a
anneal st r sel = runAnneal st r (do_anneal sel)

annealMax :: a -> (a -> Double) -> (a -> [a]) -> Rated Double a
annealMax st r sel = runAnneal st (negate . r) (do_anneal sel)

do_anneal :: (a -> [a]) -> State (Anneal a) (Rated Double a)
do_anneal sel = do sequence_ $ replicate 100 da
                   gets best
    where da = do select_metropolis sel
                  modify $ \s -> s { temperature = temperature s *0.99 }

data Anneal a = A { g :: StdGen
                  , best :: Rated Double a
                  , current :: Rated Double a
                  , rate :: a -> Rated Double a
                  , temperature :: Double }

runAnneal :: a -> (a -> Double) -> State (Anneal a) b -> b
runAnneal start r x = fst $ runState x (A { g = mkStdGen 137
                                          , best = Rated (r start) start
                                          , current = Rated (r start) start
                                          , rate = \xx -> Rated (r xx) xx
                                          , temperature = 1.0 })

select_metropolis :: (a -> [a]) -> State (Anneal a) ()
select_metropolis x = do c <- gets current
                         a <- select $ x $ the_value c
                         metropolis a

metropolis :: a -> State (Anneal a) ()
metropolis x = do r <- gets rate
                  c <- gets current
                  t <- gets temperature
                  let rx = r x
                      boltz = exp $ (the_rating c - the_rating rx) / t
                  if rx < c then do modify $ \s -> s { current = rx, best = rx }
                            else do p <- getOne (0,1)
                                    if p < boltz
                                       then modify $ \s -> s { current = rx }
                                       else return ()

select :: [a] -> State (Anneal a) a
select [] = the_value `fmap` gets best
select [x] = return x
select xs = do n <- getOne (0,length xs - 1)
               return (xs !! n)

getOne :: (Random a) => (a,a) -> State (Anneal x) a
getOne bounds = do s <- get
                   (x,g') <- return $ randomR bounds (g s)
                   put $ s { g = g' }
                   return x
