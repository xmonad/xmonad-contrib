module XMonadContrib.MagicFocus (magicFocus) where

import XMonad
import StackSet

magicFocus :: Layout -> Layout
magicFocus l = l { doLayout = \s -> (doLayout l) s . swap
                 , modifyLayout = \x -> fmap magicFocus `fmap` modifyLayout l x }

swap :: Stack a -> Stack a
swap (Stack f u d) = Stack f [] (reverse u ++ d)
