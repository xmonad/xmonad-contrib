{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups
-- Description :  Modify the description of a layout in a flexible way.
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modifier that can modify the description of its underlying
-- layout on a (hopefully) flexible way.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Renamed ( -- * Usage
                               -- $usage
                               renamed
                             , named
                             , Rename(..) ) where

import XMonad
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module by adding
--
-- > import XMonad.Layout.Renamed
--
-- to your @~\/.xmonad\/xmonad.hs@.
--
-- You can then use 'renamed' to modify the description of your
-- layouts. For example:
--
-- > myLayout = renamed [PrependWords "Awesome"] $ tiled ||| Mirror tiled ||| Full

-- | Apply a list of 'Rename' values to a layout, from left to right.
renamed :: [Rename a] -> l a -> ModifiedLayout Rename l a
renamed = ModifiedLayout . Chain

-- | Rename a layout. (Convenience alias for @renamed [Replace s]@.)
named :: String -> l a -> ModifiedLayout Rename l a
named s = renamed [Replace s]

-- | The available renaming operations
data Rename a = CutLeft Int -- ^ Remove a number of characters from the left
              | CutRight Int -- ^ Remove a number of characters from the right
              | Append String -- ^ Add a string on the right
              | Prepend String -- ^ Add a string on the left
              | CutWordsLeft Int -- ^ Remove a number of words from the left
              | CutWordsRight Int -- ^ Remove a number of words from the right
              | KeepWordsLeft Int -- ^ Keep a number of words from the left
              | KeepWordsRight Int -- ^ Keep a number of words from the right
              | AppendWords String -- ^ Add a string to the right, prepending a space to it
                                   -- if necessary
              | PrependWords String -- ^ Add a string to the left, appending a space to it if
                                    -- necessary
              | Replace String -- ^ Replace with another string
              | Chain [Rename a] -- ^ Apply a list of modifications in left-to-right order
  deriving (Show, Read, Eq)

apply :: Rename a -> String -> String
apply (CutLeft i) s = drop i s
apply (CutRight i) s = take (length s - i) s
apply (CutWordsLeft i) s = unwords $ drop i $ words s
apply (CutWordsRight i) s = let ws = words s
                           in unwords $ take (length ws - i) ws
apply (KeepWordsLeft i) s = unwords $ take i $ words s
apply (KeepWordsRight i) s = let ws = words s
                           in unwords $ drop (length ws - i) ws
apply (Replace s) _ = s
apply (Append s') s = s ++ s'
apply (Prepend s') s = s' ++ s
apply (AppendWords s') s = unwords $ words s ++ [s']
apply (PrependWords s') s = unwords $ s' : words s
apply (Chain rs) s = ($ s) $ foldr (flip (.) . apply) id rs

instance LayoutModifier Rename a where
    modifyDescription r l = apply r (description l)
