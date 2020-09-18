{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module RotateSome where

import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, listOf, quickCheck)
import Utils (applyN)
import XMonad.StackSet (Stack (Stack), down, focus, integrate, up)
import XMonad.Actions.RotateSome (rotateSome)

instance Arbitrary (Stack Int) where
  arbitrary = do
    foc   <- arbNat
    ups   <- listOf arbNat
    downs <- listOf arbNat
    pure (Stack foc ups downs)

arbNat :: Gen Int
arbNat = fmap abs arbitrary

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

main :: IO ()
main = do
  putStrLn "Testing rotateSome length"
  quickCheck prop_rotate_some_length

  putStrLn "Testing rotateSome cycle"
  quickCheck prop_rotate_some_cycle

  putStrLn "Testing rotateSome anchors"
  quickCheck prop_rotate_some_anchors

  putStrLn "Testing rotateSome rotate"
  quickCheck prop_rotate_some_rotate

  putStrLn "Testing rotateSome focus"
  quickCheck prop_rotate_some_focus
