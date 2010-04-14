{-# OPTIONS -fglasgow-exts #-}

import Data.List(find,union)
import Data.Maybe(fromJust)
import Test.QuickCheck

import XMonad.StackSet
import Properties(T, NonNegative) -- requires tests/Properties.hs from xmonad-core
import XMonad.Actions.SwapWorkspaces

-- Ensures that no "loss of information" can happen from a swap.
prop_double_swap (ss :: T) (t1 :: NonNegative Int) (t2 :: NonNegative Int) =
  t1 `tagMember` ss && t2 `tagMember` ss ==>
  ss == swap (swap ss)
  where swap = swapWorkspaces t1 t2

-- Degrade nicely when given invalid data.
prop_invalid_swap (ss :: T) (t1 :: NonNegative Int) (t2 :: NonNegative Int) =
  not (t1 `tagMember` ss || t2 `tagMember` ss) ==>
  ss == swapWorkspaces t1 t2 ss

-- This doesn't pass yet. Probably should.
-- prop_half_invalid_swap (ss :: T) (t1 :: NonNegative Int) (t2 :: NonNegative Int) =
--   t1 `tagMember` ss && not (t2 `tagMember` ss) ==>
--   ss == swapWorkspaces t1 t2 ss

zipWorkspacesWith :: (Workspace i l a -> Workspace i l a -> n) -> StackSet i l a s sd ->
                     StackSet i l a s sd -> [n]
zipWorkspacesWith f s t = f (workspace $ current s) (workspace $ current t) :
                          zipWith f (map workspace $ visible s) (map workspace $ visible t) ++
                          zipWith f (hidden s)  (hidden t)

-- Swap only modifies the workspaces tagged t1 and t2 -- leaves all others alone.
prop_swap_only_two (ss :: T) (t1 :: NonNegative Int) (t2 :: NonNegative Int) =
  t1 `tagMember` ss && t2 `tagMember` ss ==>
  and $ zipWorkspacesWith mostlyEqual ss (swapWorkspaces t1 t2 ss)
  where mostlyEqual w1 w2 = map tag [w1, w2] `elem` [[t1, t2], [t2, t1]] || w1 == w2

-- swapWithCurrent stays on current
prop_swap_with_current (ss :: T) (t :: NonNegative Int) =
  t `tagMember` ss ==>
  layout before == layout after && stack before == stack after
  where before = workspace $ current ss
        after  = workspace $ current $ swapWithCurrent t ss

main = do
  putStrLn "Testing double swap"
  quickCheck prop_double_swap
  putStrLn "Testing invalid swap"
  quickCheck prop_invalid_swap
  -- putStrLn "Testing half-invalid swap"
  -- quickCheck prop_half_invalid_swap
  putStrLn "Testing swap only two"
  quickCheck prop_swap_only_two
  putStrLn "Testing swap with current"
  quickCheck prop_swap_with_current
