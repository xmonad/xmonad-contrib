{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------
--
-- Tests for XPrompt and ShellPrompt
--
-------------------------------------
module XPrompt where

import Data.Char
import Test.QuickCheck

import Data.List

import XMonad.Prompt
import qualified XMonad.Prompt.Shell as S
import Properties
 
{-
instance Arbitrary Char where
    arbitrary     = choose ('\32', '\255')
    coarbitrary c = variant (ord c `rem` 4)

-}

doubleCheck p = check (defaultConfig { configMaxTest = 1000}) p
deepCheck p = check (defaultConfig { configMaxTest = 10000}) p
deepestCheck p = check (defaultConfig { configMaxTest = 100000}) p

-- brute force check for exceptions
prop_split (str :: [Char]) =
    forAll (elements str) $ \e -> S.split e str == S.split e str

-- check if the first element of the new list is indeed the first part
-- of the string.
prop_spliInSubListsAt (x :: Int) (str :: [Char]) =
    x < length str ==> result == take x str
    where result = case splitInSubListsAt x str of
                     [] -> []
                     x -> head x

-- skipLastWord is complementary to getLastWord, unless the only space
-- in the string is the final character, in which case skipLastWord
-- and getLastWord will produce the same result.
prop_skipGetLastWord (str :: [Char]) =
    skipLastWord str ++ getLastWord str == str || skipLastWord str == getLastWord str


-- newIndex and newCommand get only non empy lists
elemGen :: Gen ([String],String)
elemGen = do
  a <- arbitrary :: Gen [[Char]]
  let l = case filter (/= []) a of
            [] -> ["a"]
            x -> x
  e <- elements l
  return (l,e)

{- newIndex and newCommand have since been renamed or are no longer used

-- newIndex calculates the index of the next completion in the
-- completion list, so the index must be within the range of the
-- copletions list
prop_newIndex_range =
    forAll elemGen $ \(l,c) -> newIndex c l >= 0 &&  newIndex c l < length l
-}

-- this is actually the definition of newCommand...
-- just to check something.
{-
prop_newCommandIndex = 
    forAll elemGen $ \(l,c) -> (skipLastWord c ++ (l !! (newIndex c l)))  == newCommand c l
-}

main = do
  putStrLn "Testing ShellPrompt.split"
  deepCheck prop_split
  putStrLn "Testing spliInSubListsAt"
  deepCheck prop_spliInSubListsAt
  putStrLn "Testing skip + get lastWord"
  deepCheck prop_skipGetLastWord

