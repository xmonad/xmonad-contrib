--------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Prompt.FuzzyMatch
-- Description : A prompt for fuzzy completion matching in prompts akin to Emacs ido-mode.
-- Copyright   : (C) 2015 Norbert Zeh
-- License     : GPL
--
-- Maintainer  : Norbert Zeh <norbert.zeh@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A module for fuzzy completion matching in prompts akin to emacs ido mode.
--
--------------------------------------------------------------------------------

module XMonad.Prompt.FuzzyMatch ( -- * Usage
                                  -- $usage
                                  fuzzyMatch
                                , fuzzySort
                                ) where

import XMonad.Prelude
import qualified Data.List.NonEmpty as NE

-- $usage
--
-- This module offers two aspects of fuzzy matching of completions offered by
-- XMonad.Prompt.
--
-- 'fuzzyMatch' can be used as the searchPredicate in the XPConfig.  The effect
-- is that any completion that contains the currently typed characters as a
-- subsequence is a valid completion; matching is case insensitive.  This means
-- that the sequence of typed characters can be obtained from the completion by
-- deleting an appropriate subset of its characters.  Example: "spr" matches
-- "FastSPR" but also "SuccinctParallelTrees" because it's a subsequence of the
-- latter: "S.......P.r..........".
--
-- While this type of inclusiveness is helpful most of the time, it sometimes
-- also produces surprising matches.  'fuzzySort' helps sorting matches by
-- relevance, using a simple heuristic for measuring relevance.  The matches are
-- sorted primarily by the length of the substring that contains the query
-- characters and secondarily the starting position of the match.  So, if the
-- search string is "spr" and the matches are "FastSPR", "FasterSPR", and
-- "SuccinctParallelTrees", then the order is "FastSPR", "FasterSPR",
-- "SuccinctParallelTrees" because both "FastSPR" and "FasterSPR" contain "spr"
-- within a substring of length 3 ("SPR") while the shortest substring of
-- "SuccinctParallelTrees" that matches "spr" is "SuccinctPar", which has length
-- 11.  "FastSPR" is ranked before "FasterSPR" because its match starts at
-- position 5 while the match in "FasterSPR" starts at position 7.
--
-- To use these functions in an XPrompt, for example, for windowPrompt:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Window ( windowPrompt )
-- > import XMonad.Prompt.FuzzyMatch
-- >
-- > myXPConfig = def { searchPredicate = fuzzyMatch
-- >                  , sorter          = fuzzySort
-- >                  }
--
-- then add this to your keys definition:
--
-- > , ((modm .|. shiftMask, xK_g), windowPrompt myXPConfig Goto allWindows)
--
-- For detailed instructions on editing the key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Returns True if the first argument is a subsequence of the second argument,
-- that is, it can be obtained from the second sequence by deleting elements.
fuzzyMatch :: String -> String -> Bool
fuzzyMatch a b = isSubsequenceOf (map toLower a) (map toLower b)

-- | Sort the given set of strings by how well they match.  Match quality is
-- measured first by the length of the substring containing the match and second
-- by the positions of the matching characters in the string.
fuzzySort :: String -> [String] -> [String]
fuzzySort q = map snd . sort . map (rankMatch q)

rankMatch :: String -> String -> ((Int, Int), String)
rankMatch q s = (if null matches then (maxBound, maxBound) else minimum matches, s)
  where matches = rankMatches q s

rankMatches :: String -> String -> [(Int, Int)]
rankMatches [] _ = [(0, 0)]
rankMatches (q:qs) s = map (\(l, r) -> (r - l, l)) $ findShortestMatches (q :| qs) s

findShortestMatches :: NonEmpty Char -> String -> [(Int, Int)]
findShortestMatches q s = foldl' extendMatches spans oss
  where (os :| oss) = NE.map (findOccurrences s) q
        spans       = [(o, o) | o <- os]

findOccurrences :: String -> Char -> [Int]
findOccurrences s c = map snd $ filter ((toLower c ==) . toLower . fst) $ zip s [0..]

extendMatches :: [(Int, Int)] -> [Int] -> [(Int, Int)]
extendMatches spans = map last . groupBy ((==) `on` snd) . extendMatches' spans

extendMatches' :: [(Int, Int)] -> [Int] -> [(Int, Int)]
extendMatches' []                    _          = []
extendMatches' _                     []         = []
extendMatches' spans@((l, r):spans') xs@(x:xs') | r < x     = (l, x) : extendMatches' spans' xs
                                                | otherwise = extendMatches' spans xs'
