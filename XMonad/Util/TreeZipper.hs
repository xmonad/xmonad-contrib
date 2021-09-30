{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TreeSelect
-- Description :  Zipper over "Data.Tree".
-- Copyright   :  (c) Tom Smeets <tom.tsmeets@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Tom Smeets <tom.tsmeets@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- <https://wiki.haskell.org/Zipper Zipper> over the "Data.Tree" data structure.
-- This module is based on <http://hackage.haskell.org/package/rosezipper rosezipper>.
--
-----------------------------------------------------------------------------

module XMonad.Util.TreeZipper(
    -- * Data structure
      TreeZipper(..)
    , cursor

    -- * Conversion
    , fromForest
    , toForest
    , getSubForest

    -- * Navigation
    , rootNode
    , parent
    , children
    , nextChild
    , previousChild

    -- * Utils
    , nodeDepth
    , nodeIndex
    , followPath
    , findChild

    , isLeaf
    , isRoot
    , isLast
    , isFirst
    ) where

import Data.Tree

-- | A <https://wiki.haskell.org/Zipper Zipper> over the "Data.Tree" data structure.
data TreeZipper a = TreeZipper { tz_current :: Tree a -- ^ the currently focused sub-tree under the cursor
                               , tz_before  :: Forest a -- ^ all sub-tree's to the /left/ of the cursor that have the same parent
                               , tz_after   :: Forest a -- ^ all sub-tree's to the /right/ of the cursor that have the same parent
                               , tz_parents :: [(Forest a, a, Forest a)] -- ^ list zippers for each parent level, the first element is the current parent
                               }
-- ^ Very crappy visualization of the 'TreeZipper' data structure
--
-- @
--              (tz_parents)
--        ([*],       *, [*])
--        ([*, *],    *, [])
--        ([],        *                  [*,   *])
--                    |                   |    |
--   +-------+--------+-------+------+  +-*-+  *
--   |       |        |       |      |  |   |
--  (tz_before) (tz_current) (tz_after) *   *
--   |       |                |      |
-- +-*-+     *                *      *
-- |   |
-- *   *
-- @

-- | Get the highlighted value
cursor :: TreeZipper a -> a
cursor = rootLabel . tz_current

-- | Create a 'TreeZipper' from a list of 'Data.Tree.Tree's focused on the first element
fromForest :: Forest a -> TreeZipper a
fromForest [] = error "XMonad.Util.TreeZipper.fromForest: can't create a TreeZipper from an empty list!"
fromForest (x:xs) = TreeZipper { tz_current = x
                               , tz_before  = []
                               , tz_after   = xs
                               , tz_parents = []
                               }

-- | Convert the entire zipper back to a 'Data.Tree.Forest'
toForest :: TreeZipper a -> Forest a
toForest = getSubForest . rootNode

-- | Create a 'Data.Tree.Forest' from all the children of the current parent
getSubForest :: TreeZipper a -> Forest a
getSubForest TreeZipper{..} = reverse tz_before ++ tz_current : tz_after

-- | Go to the upper most node such that
-- nothing is before nor above the cursor
rootNode :: TreeZipper a -> TreeZipper a
rootNode = f
  where
    f z = maybe (g z) f $ parent z
    g z = maybe z g $ previousChild z

-- | Move to the parent node
parent :: TreeZipper a -> Maybe (TreeZipper a)
parent t = case tz_parents t of
    (xs,a,ys) : ps -> Just
        TreeZipper { tz_current  = Node a (reverse (tz_before t) ++ tz_current t : tz_after t)
                   , tz_before   = xs
                   , tz_after    = ys
                   , tz_parents  = ps
                   }
    [] -> Nothing

-- | Move the cursor one level down to the first node
children :: TreeZipper a -> Maybe (TreeZipper a)
children z = case subForest $ tz_current z of
    (n:xs) -> Just
        TreeZipper { tz_current = n
                   , tz_before  = []
                   , tz_after   = xs
                   , tz_parents = (tz_before z, cursor z, tz_after z) : tz_parents z
                   }
    [] -> Nothing

-- | Go to the next child node
nextChild :: TreeZipper a -> Maybe (TreeZipper a)
nextChild z = case tz_after z of
    (n:xs) -> Just
        TreeZipper { tz_current = n
                   , tz_before  = tz_current z : tz_before z
                   , tz_after   = xs
                   , tz_parents = tz_parents z
                   }
    [] -> Nothing

-- | Go to the previous child node
previousChild :: TreeZipper a -> Maybe (TreeZipper a)
previousChild z = case tz_before z of
    (n:xs) -> Just
        TreeZipper { tz_current = n
                   , tz_before  = xs
                   , tz_after   = tz_current z : tz_after z
                   , tz_parents = tz_parents z
                   }
    [] -> Nothing

-- | How many nodes are above this one?
nodeDepth :: TreeZipper a -> Int
nodeDepth = length . tz_parents

-- | How many nodes are before the cursor? (on the current level)
nodeIndex :: TreeZipper a -> Int
nodeIndex = length . tz_before

-- | follow a Path specified by the list of nodes
followPath :: Eq b => (a -> b) -> [b] -> TreeZipper a -> Maybe (TreeZipper a)
followPath _ []     z = Just z
followPath f [x]    z = findChild (\y -> f y == x) z
followPath f (x:xs) z = findChild (\y -> f y == x) z >>= children >>= followPath f xs

-- | go to the first node next to the cursor that matches
findChild :: (a -> Bool) -> TreeZipper a -> Maybe (TreeZipper a)
findChild f z | f (cursor z) = Just z
              | otherwise    = nextChild z >>= findChild f

-- | Check whenther this is a leaf node
isLeaf :: TreeZipper a -> Bool
isLeaf = null . subForest . tz_current

-- | Check whenther this is a leaf node
isRoot :: TreeZipper a -> Bool
isRoot = null . tz_parents

-- | Check whenther this the last child
isLast :: TreeZipper a -> Bool
isLast = null . tz_after

-- | Check whenther this the first child
isFirst :: TreeZipper a -> Bool
isFirst = null . tz_before
