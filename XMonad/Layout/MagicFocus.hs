{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.MagicFocus
-- Copyright    : (c) Peter De Wachter <pdewacht@gmail.com>
-- License      : BSD
--
-- Maintainer   : Peter De Wachter <pdewacht@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Automagically put the focused window in the master area.
-----------------------------------------------------------------------------

module XMonad.Layout.MagicFocus
    (-- * Usage
     -- $usage
     magicFocus
    ) where

import XMonad
import XMonad.StackSet
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MagicFocus
--
-- Then edit your @layoutHook@ by adding the magicFocus layout
-- modifier:
--
-- > myLayouts = magicFocus (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Create a new layout which automagically puts the focused window
--   in the master area.
magicFocus :: l a -> ModifiedLayout MagicFocus l a
magicFocus = ModifiedLayout MagicFocus

data MagicFocus a = MagicFocus deriving (Show, Read)

instance LayoutModifier MagicFocus Window where
  modifyLayout MagicFocus (Workspace i l s) r =
    withWindowSet $ \wset ->
      runLayout (Workspace i l (s >>= \st -> Just $ swap st (peek wset))) r

swap :: (Eq a) => Stack a -> Maybe a -> Stack a
swap (Stack f u d) focused | Just f == focused = Stack f [] (reverse u ++ d)
                           | otherwise         = Stack f u d
