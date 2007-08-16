-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.MagicFocus
-- Copyright    : (c) Peter De Wachter <pdewacht@gmail.com>
-- License      : BSD
--
-- Maintainer   : Peter De Wachter <pdewacht@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Automagically put the focused window in the master area.
-----------------------------------------------------------------------------

module XMonadContrib.MagicFocus (
                 -- * Usage
                 -- $usage
                 magicFocus) where

import Graphics.X11.Xlib (Window)
import XMonad
import StackSet

-- $usage
-- > import XMonadContrib.MagicFocus
-- > defaultLayouts = [ magicFocus tiled , magicFocus $ mirror tiled ]

magicFocus :: Layout Window -> Layout Window
magicFocus l = l { doLayout = \r s -> withWindowSet (return . peek) >>= (doLayout l) r . swap s
                 , modifyLayout = \x -> fmap magicFocus `fmap` modifyLayout l x }

swap :: (Eq a) => Stack a -> Maybe a -> Stack a
swap (Stack f u d) focused | Just f == focused = Stack f [] (reverse u ++ d)
                           | otherwise         = Stack f u d
