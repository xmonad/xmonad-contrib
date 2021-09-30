-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Minimize
-- Description :  Common utilities for window minimizing\/maximizing.
-- Copyright   :  (c) Bogdan Sinitsyn (2016)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  bogdan.sinitsyn@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Stores some common utilities for modules used for window minimizing/maximizing
--
-----------------------------------------------------------------------------
module XMonad.Util.Minimize
  ( RectMap
  , Minimized(..)
  ) where

import XMonad
import qualified XMonad.StackSet as W

import qualified Data.Map as M

type RectMap = M.Map Window (Maybe W.RationalRect)

data Minimized = Minimized
    { rectMap :: RectMap
    , minimizedStack :: [Window]
    }
    deriving (Eq, Read, Show)

instance ExtensionClass Minimized where
  initialValue = Minimized { rectMap = M.empty
                           , minimizedStack = []
                           }
  extensionType = PersistentExtension
