{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RotateSome where

import Utils
import Test.QuickCheck (Arbitrary, arbitrary, choose)
import XMonad.StackSet (Stack, integrate, up)
import XMonad.Actions.RotateSome (rotateSome)

newtype Divisor = Divisor Int deriving Show
instance Arbitrary Divisor where
  arbitrary = Divisor <$> choose (1, 5)

isMultOf :: Int -> Int -> Bool
x `isMultOf` n = (x `rem` n) == 0

-- Total number of elements does not change.
prop_rotate_some_length (Divisor d) (stk :: Stack Int) =
  length (integrate stk) == length (integrate $ rotateSome (`isMultOf` d) stk)

-- Applying rotateSome N times completes a cycle, where N is the number of
-- elements that satisfy the predicate.
prop_rotate_some_cycle (Divisor d) (stk :: Stack Int) =
  stk == applyN (Just n) (rotateSome (`isMultOf` d)) stk
  where
    n = length $ filter (`isMultOf` d) (integrate stk)

-- Elements that do not satisfy the predicate remain anchored in place.
prop_rotate_some_anchors (Divisor d) (stk :: Stack Int) =
  all check $
    zip
      (integrate stk)
      (integrate $ rotateSome (`isMultOf` d) stk)
  where
    check (before, after) = (before `isMultOf` d) || before == after

-- Elements that satisfy the predicate rotate by one position.
prop_rotate_some_rotate (Divisor d) (stk :: Stack Int) =
  drop 1 before ++ take 1 before == after
  where
    before = filter p (integrate stk)
    after = filter p (integrate $ rotateSome p stk)
    p = (`isMultOf` d)

-- Focus position is preserved.
prop_rotate_some_focus (Divisor d) (stk :: Stack Int) =
  length (up stk) == length (up $ rotateSome (`isMultOf` d) stk)
