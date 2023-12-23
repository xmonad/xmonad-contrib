{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Simplest
-- Description :  A very simple layout.
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A very simple layout. The simplest, afaik.
-----------------------------------------------------------------------------

module XMonad.Layout.Simplest
    ( -- * Usage:
      -- $usage
      Simplest (..)
    ) where

import XMonad
import qualified XMonad.StackSet as S

-- $usage
-- You can use this module with the following in your
-- @xmonad.hs@:
--
-- > import XMonad.Layout.Simplest
--
-- Then edit your @layoutHook@ by adding the Simplest layout:
--
-- > myLayout = Simplest ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".

data Simplest a = Simplest deriving (Show, Read)
instance LayoutClass Simplest a where
    pureLayout Simplest rec (S.Stack w l r) = map (, rec) (w : reverse l ++ r)
