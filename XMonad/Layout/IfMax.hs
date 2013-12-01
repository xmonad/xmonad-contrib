-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IfMax
-- Copyright   :  (c) 2013 Ilya Portnov
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides IfMax layout, which will run one layout if there are maximum N 
-- windows on workspace, and another layout, when number of windows is greater
-- than N.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.IfMax
    ( -- * Usage
      -- $usage
      IfMax (..)
    , ifMax
    ) where

import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W

-- $usage
-- IfMax layout will run one layout if number of windows on workspace is as 
-- maximum N, and else will run another layout.
--
-- You can use this module by adding folowing in your @xmonad.hs@:
--
-- > import XMonad.Layout.IfMax
--
-- Then add layouts to your layoutHook:
--
-- > myLayoutHook = IfMax 2 Full (Tall ...) ||| ...
--
-- In this example, if there are 1 or 2 windows, Full layout will be used;
-- otherwise, Tall layout will be used.
--

data IfMax l1 l2 w = IfMax Int (l1 w) (l2 w)
  deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a)
    => LayoutClass (IfMax l1 l2) a where

  runLayout (W.Workspace _ (IfMax n l1 l2) s) rect = arrange (W.integrate' s)
    where
      arrange [] = do l1' <- maybe l1 id `fmap` handleMessage l1 (SomeMessage ReleaseResources)
                      l2' <- maybe l2 id `fmap` handleMessage l2 (SomeMessage ReleaseResources)
                      return ([], Just $ IfMax n l1' l2')
      arrange ws | length ws <= n = do
                                    (wrs, ml1') <- runLayout (W.Workspace "" l1 s) rect
                                    let l1' = fromMaybe l1 ml1'
                                    return (wrs, Just $ IfMax n l1' l2)
                 | otherwise      = do
                                    (wrs, ml2') <- runLayout (W.Workspace "" l2 s) rect
                                    let l2' = fromMaybe l2 ml2'
                                    return (wrs, Just $ IfMax n l1 l2')

  description (IfMax n l1 l2) = "If number of windows is <= " ++ show n ++ ", then " ++
                                description l1 ++ ", else " ++ description l2

-- | Layout itself
ifMax :: (LayoutClass l1 w, LayoutClass l2 w)
      => Int            -- ^ Maximum number of windows for the first layout
      -> l1 w           -- ^ First layout
      -> l2 w           -- ^ Second layout
      -> IfMax l1 l2 w
ifMax n l1 l2 = IfMax n l1 l2

