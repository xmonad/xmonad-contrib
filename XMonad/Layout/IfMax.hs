-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IfMax
-- Description :  Decide upon a layout depending on the number of windows.
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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, PatternGuards #-}

module XMonad.Layout.IfMax
    ( -- * Usage
      -- $usage
      IfMax (..)
    , ifMax
    ) where

import Control.Arrow ((&&&))
import qualified Data.List as L
import qualified Data.Map  as M

import XMonad
import XMonad.Prelude
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

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (IfMax l1 l2) Window where

  runLayout (W.Workspace wname (IfMax n l1 l2) s) rect = withWindowSet $ \ws -> arrange (W.integrate' s) (M.keys . W.floating $ ws)
    where
      arrange ws fw | length (ws L.\\ fw) <= n = do
                                    (wrs, ml1') <- runLayout (W.Workspace wname l1 s) rect
                                    let l1' = fromMaybe l1 ml1'
                                    l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage Hide)
                                    return (wrs, Just $ IfMax n l1' l2')
                    | otherwise      = do
                                    (wrs, ml2') <- runLayout (W.Workspace wname l2 s) rect
                                    l1' <- fromMaybe l1 <$> handleMessage l1 (SomeMessage Hide)
                                    let l2' = fromMaybe l2 ml2'
                                    return (wrs, Just $ IfMax n l1' l2')

  handleMessage (IfMax n l1 l2) m | Just ReleaseResources <- fromMessage m = do
      l1' <- handleMessage l1 (SomeMessage ReleaseResources)
      l2' <- handleMessage l2 (SomeMessage ReleaseResources)
      if isNothing l1' && isNothing l2'
         then return Nothing
         else return $ Just $ IfMax n (fromMaybe l1 l1') (fromMaybe l2 l2')
  handleMessage (IfMax n l1 l2) m = do
      (allWindows, floatingWindows) <- gets ((W.integrate' . W.stack . W.workspace . W.current &&& M.keys . W.floating) . windowset)
      if length (allWindows L.\\ floatingWindows) <= n
        then do
          l1' <- handleMessage l1 m
          return $ flip (IfMax n) l2 <$> l1'
        else do
          l2' <- handleMessage l2 m
          return $ IfMax n l1 <$> l2'

  description (IfMax n l1 l2) = "If number of windows is <= " ++ show n ++ ", then " ++
                                description l1 ++ ", else " ++ description l2

-- | Layout itself
ifMax :: (LayoutClass l1 w, LayoutClass l2 w)
      => Int            -- ^ Maximum number of windows for the first layout
      -> l1 w           -- ^ First layout
      -> l2 w           -- ^ Second layout
      -> IfMax l1 l2 w
ifMax = IfMax
