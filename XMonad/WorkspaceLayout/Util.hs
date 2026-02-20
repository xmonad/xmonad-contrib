module XMonad.WorkspaceLayout.Util where

(!%) :: [a] -> Int -> a
xs !% n = xs !! (n `mod` length xs)

-- Doubly-inclusive
affineMod :: (Ord a, Num a) => (a, a) -> (a -> a)
affineMod range@(lo, hi) x
  | x > hi = affineMod range (x - (hi - lo + 1))
  | x < lo = affineMod range (x + (hi - lo + 1))
  | otherwise = x
