{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PerScreen
-- Description :  Configure layouts based on the screen rectangle.
-- Copyright   :  (c) Brandon S. Allbery KF8NH
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <allbery.b@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts based on the screen rectangle passed to the layout.
-- This gives you true per-screen functionality.
--
-- The old PerScreen is now X.L.ByWidth. We re-export it deprecated for
-- backward compatibility.
-----------------------------------------------------------------------------

module XMonad.Layout.PerScreen
    ( -- * Usage
      -- $usage
      OnScreen,
      onScreen,
      onScreens,
      -- * Deprecated
      -- $deprecated
      IW.PerScreen,
      ifWider
    ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Prelude (fromMaybe, fi)

import qualified XMonad.Layout.IfWidth as IW

import Data.List (find)

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.OnScreen
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = onScreen 1 (Tall 1 (3/100) (1/2) ||| Full) Full
--
-- Replace any of the layouts with any arbitrarily complicated layout.
-- 'onScreen' can also be used inside other layout combinators, although the
-- result may be confusing.

-- | Specify a layout to run on a given screen.
onScreen :: (LayoutClass l1 a, LayoutClass l2 a)
         => ScreenId -> l1 a -> l2 a -> OnScreen l1 l2 a
onScreen s = onScreens [s]

-- | Specify a layout to run on a list of screens.
--   Note that this works by 'ScreenId'. It has a 'Num' instance, so literal
--   screen numbers will work as expected, but if you use a binding you need
--   to use the 'S' constructor.
onScreens :: (LayoutClass l1 a, LayoutClass l2 a)
          => [ScreenId] -> l1 a -> l2 a -> OnScreen l1 l2 a
onScreens ss l1 l2 = OnScreen ss l1 l2 False

data OnScreen l1 l2 a = OnScreen [ScreenId] (l1 a) (l2 a) Bool
    deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (OnScreen l1 l2) a where
    runLayout (W.Workspace i p@(OnScreen ss l1 l2 _) ms) r = do
        which <- withWindowSet $ \ws -> do
            let srs = sinfo (W.current ws) : map sinfo (W.visible ws)
                f lr (_,sr) = rect_x lr >= rect_x sr &&
                              rect_x lr < rect_x sr + fi (rect_width sr) &&
                              rect_y lr >= rect_y sr &&
                              rect_y lr < rect_y sr + fi (rect_height sr)
                sinfo (W.Screen _ sid (SD sd)) = (sid, sd)
            return $ maybe 0 fst (find (f r) srs) `elem` ss
        if which
            then do handleMessage l2 (SomeMessage Hide)
                    (wrs, mlt') <- runLayout (W.Workspace i l1 ms) r
                    return (wrs, Just $ updateL1 p mlt')
            else do handleMessage l1 (SomeMessage Hide)
                    (wrs, mlt') <- runLayout (W.Workspace i l2 ms) r
                    return (wrs, Just $ updateL2 p mlt')

    handleMessage (OnScreen ss l1 l2 b) m
        | fromMessage m == Just Hide = do
            l1' <- handleMessage l1 m
            l2' <- handleMessage l2 m
            return $ Just $ OnScreen ss (fromMaybe l1 l1') (fromMaybe l2 l2') b
        | fromMessage m == Just ReleaseResources = do
            l1' <- handleMessage l1 m
            l2' <- handleMessage l2 m
            return $ Just $ OnScreen ss (fromMaybe l1 l1') (fromMaybe l2 l2') b
        | b = handleMessage l1 m >>= maybe (return Nothing) (\nl1 -> return . Just $ OnScreen ss nl1 l2 b)
        | otherwise = handleMessage l2 m >>= maybe (return Nothing) (\nl2 -> return . Just $ OnScreen ss l1 nl2 b)

    description (OnScreen _ l1 _  True ) = description l1
    description (OnScreen _ _  l2 False) = description l2

updateL1 :: OnScreen l1 l2 a -> Maybe (l1 a) -> OnScreen l1 l2 a
updateL1 (OnScreen ss l1 l2 _) mlt = OnScreen ss (fromMaybe l1 mlt) l2 True

updateL2 :: OnScreen l1 l2 a -> Maybe (l2 a) -> OnScreen l1 l2 a
updateL2 (OnScreen ss l1 l2 _) mlt = OnScreen ss l1 (fromMaybe l2 mlt) False

-- $deprecated
-- Older versions of this module exported an 'ifWidth' layout modifier. This
-- has been moved to 'XMonad.Layout.IfWidth', but is re-exported for backward
-- compatibility. It is deprecated and will be removed in favor of 'IfWidth'
-- in a future release.

ifWider :: (LayoutClass l1 a, LayoutClass l2 a)
               => Dimension   -- ^ target screen width
               -> l1 a        -- ^ layout to use when the screen is wide enough
               -> l2 a        -- ^ layout to use otherwise
               -> IW.PerScreen l1 l2 a
ifWider = IW.ifWider
{-# DEPRECATED ifWider "Use XMonad.Layout.IfWidth.ifWider instead" #-}
