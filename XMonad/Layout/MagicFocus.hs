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
     MagicFocus(MagicFocus)
    ) where

import XMonad
import XMonad.StackSet

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MagicFocus
--
-- Then edit your @layoutHook@ by adding the MagicFocus layout
-- modifier:
--
-- > myLayouts = MagicFocus (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data MagicFocus l a = MagicFocus (l a) deriving ( Show , Read )

instance (LayoutClass l Window) => LayoutClass (MagicFocus l) Window where
          doLayout = magicFocus

magicFocus :: LayoutClass l Window => MagicFocus l Window -> Rectangle
           -> Stack Window -> X ([(Window, Rectangle)], Maybe (MagicFocus l Window))
magicFocus (MagicFocus l) r s =
    withWindowSet $ \wset -> do
      (ws,nl) <- runLayout (Workspace "" l (Just . swap s $ peek wset)) r
      case nl of
        Nothing -> return (ws, Nothing)
        Just l' -> return (ws, Just $ MagicFocus l')

swap :: (Eq a) => Stack a -> Maybe a -> Stack a
swap (Stack f u d) focused | Just f == focused = Stack f [] (reverse u ++ d)
                           | otherwise         = Stack f u d
