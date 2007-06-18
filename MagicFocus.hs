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
                 -- $ usage
                 magicFocus) where

import XMonad
import StackSet

-- $ usage
-- > import XMonadContrib.MagicFocus
-- > defaultLayouts = [ magicFocus tiled , magicFocus $ mirror tiled ]

magicFocus :: Layout -> Layout
magicFocus l = l { doLayout = \s -> (doLayout l) s . swap
                 , modifyLayout = \x -> fmap magicFocus `fmap` modifyLayout l x }

swap :: Stack a -> Stack a
swap (Stack f u d) = Stack f [] (reverse u ++ d)
