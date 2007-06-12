module XMonadContrib.MagicFocus (magicFocus) where

import XMonad
import StackSet

magicFocus l = l { doLayout = \s -> (doLayout l) s . swap
                 , modifyLayout = \x -> fmap magicFocus `fmap` modifyLayout l x }

swap :: Stack a -> Stack a
swap Empty = Empty
swap (Node f u d) = Node f [] (reverse u ++ d)
