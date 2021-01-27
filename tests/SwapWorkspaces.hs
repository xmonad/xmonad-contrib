{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SwapWorkspaces where
import Instances
import Test.QuickCheck

import XMonad.StackSet
import XMonad.Actions.SwapWorkspaces


-- Ensures that no "loss of information" can happen from a swap.
prop_double_swap (ss :: T) = do
  t1 <- arbitraryTag ss
  t2 <- arbitraryTag ss
  let swap = swapWorkspaces t1 t2
  return $ ss == swap (swap ss)

-- Degrade nicely when given invalid data.
prop_invalid_swap (ss :: T) = do
  t1 <- arbitrary `suchThat` (not . (`tagMember` ss))
  t2 <- arbitrary `suchThat` (not . (`tagMember` ss))
  return $ ss == swapWorkspaces t1 t2 ss

-- This doesn't pass yet. Probably should.
-- prop_half_invalid_swap (ss :: T) (NonNegative t1) (NonNegative t2) =
--   t1 `tagMember` ss && not (t2 `tagMember` ss) ==>
--   ss == swapWorkspaces t1 t2 ss

zipWorkspacesWith :: (Workspace i l a -> Workspace i l a -> n) -> StackSet i l a s sd ->
                     StackSet i l a s sd -> [n]
zipWorkspacesWith f s t = f (workspace $ current s) (workspace $ current t) :
                          zipWith f (map workspace $ visible s) (map workspace $ visible t) ++
                          zipWith f (hidden s)  (hidden t)

-- Swap only modifies the workspaces tagged t1 and t2 -- leaves all others alone.
prop_swap_only_two (ss :: T) = do
  t1 <- arbitraryTag ss
  t2 <- arbitraryTag ss
  let mostlyEqual w1 w2 = map tag [w1, w2] `elem` [[t1, t2], [t2, t1]] || w1 == w2
  return $ and $ zipWorkspacesWith mostlyEqual ss (swapWorkspaces t1 t2 ss)

-- swapWithCurrent stays on current
prop_swap_with_current (ss :: T) = do
  t <- arbitraryTag ss
  let before = workspace $ current ss
  let after  = workspace $ current $ swapWithCurrent t ss
  return $ layout before == layout after && stack before == stack after
