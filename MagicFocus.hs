{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

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

module XMonadContrib.MagicFocus 
    (-- * Usage
     -- $usage
     MagicFocus(MagicFocus)
    ) where

import Graphics.X11.Xlib
import XMonad
import StackSet

-- $usage
-- > import XMonadContrib.MagicFocus
-- > layouts = [ Layout $ MagicFocus tiled , Layout $ MagicFocus $ Mirror tiled ]

-- %import XMonadContrib.MagicFocus
-- %layout , Layout $ MagicFocus tiled
-- %layout , Layout $ MagicFocus $ Mirror tiled


data MagicFocus l a = MagicFocus (l a) deriving ( Show , Read )

instance (LayoutClass l Window) => LayoutClass (MagicFocus l) Window where
          doLayout = magicFocus

magicFocus :: LayoutClass l Window => MagicFocus l Window -> Rectangle
           -> Stack Window -> X ([(Window, Rectangle)], Maybe (MagicFocus l Window))
magicFocus (MagicFocus l) r s = 
    withWindowSet $ \wset -> do
      (ws,nl) <- doLayout l r (swap s $ peek wset)
      case nl of
        Nothing -> return (ws, Nothing)
        Just l' -> return (ws, Just $ MagicFocus l')

swap :: (Eq a) => Stack a -> Maybe a -> Stack a
swap (Stack f u d) focused | Just f == focused = Stack f [] (reverse u ++ d)
                           | otherwise         = Stack f u d
