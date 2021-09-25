{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PerScreen
-- Description :  Configure layouts based on the width of your screen.
-- Copyright   :  (c) Edward Z. Yang
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <ezyang@cs.stanford.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts based on the width of your screen; use your
-- favorite multi-column layout for wide screens and a full-screen
-- layout for small ones.
-----------------------------------------------------------------------------

module XMonad.Layout.PerScreen
    ( -- * Usage
      -- $usage
      PerScreen,
      ifWider
    ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Prelude (fromMaybe)

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.PerScreen
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = ifWider 1280 (Tall 1 (3/100) (1/2) ||| Full) Full
--
-- Replace any of the layouts with any arbitrarily complicated layout.
-- ifWider can also be used inside other layout combinators.

ifWider :: (LayoutClass l1 a, LayoutClass l2 a)
               => Dimension   -- ^ target screen width
               -> l1 a        -- ^ layout to use when the screen is wide enough
               -> l2 a        -- ^ layout to use otherwise
               -> PerScreen l1 l2 a
ifWider w = PerScreen w False

data PerScreen l1 l2 a = PerScreen Dimension Bool (l1 a) (l2 a) deriving (Read, Show)

-- | Construct new PerScreen values with possibly modified layouts.
mkNewPerScreenT :: PerScreen l1 l2 a -> Maybe (l1 a) ->
                      PerScreen l1 l2 a
mkNewPerScreenT (PerScreen w _ lt lf) mlt' =
    (\lt' -> PerScreen w True lt' lf) $ fromMaybe lt mlt'

mkNewPerScreenF :: PerScreen l1 l2 a -> Maybe (l2 a) ->
                      PerScreen l1 l2 a
mkNewPerScreenF (PerScreen w _ lt lf) mlf' =
    PerScreen w False lt $ fromMaybe lf mlf'

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (PerScreen l1 l2) a where
    runLayout (W.Workspace i p@(PerScreen w _ lt lf) ms) r
        | rect_width r > w    = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                                   return (wrs, Just $ mkNewPerScreenT p mlt')
        | otherwise           = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                                   return (wrs, Just $ mkNewPerScreenF p mlt')

    handleMessage (PerScreen w bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ PerScreen w bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (return . Just . PerScreen w bool lt)

    description (PerScreen _ True  l1 _) = description l1
    description (PerScreen _ _     _ l2) = description l2
