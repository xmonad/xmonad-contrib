{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Combo
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that combines multiple layouts.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Combo (
                            -- * Usage
                            -- $usage 
                            combineTwo,
                            CombineTwo
                           ) where

import Data.List ( delete, intersect, (\\) )
import Data.Maybe ( isJust )
import XMonad
import XMonad.StackSet ( integrate, Stack(..) )
import XMonad.Layout.WindowNavigation ( MoveWindowToWindow(..) )
import qualified XMonad.StackSet as W ( differentiate )

-- $usage
--
-- To use this layout write, in your Config.hs:
-- 
-- > import XMonad.Layout.Combo 
-- 
-- and add something like
-- 
-- > combineTwo (TwoPane 0.03 0.5) (tabbed shrinkText defaultTConf) (tabbed shrinkText defaultTConf)
--
-- to your layouts.

-- combineTwo is a new simple layout combinator.  It allows the combination
-- of two layouts using a third to split the screen between the two, but
-- has the advantage of allowing you to dynamically adjust the layout, in
-- terms of the number of windows in each sublayout.  To do this, use
-- WindowNavigation, and add the following key bindings (or something
-- similar):

--     , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)

-- These bindings will move a window into the sublayout that is
-- up/down/left/right of its current position.  Note that there is some
-- weirdness in combineTwo, in that the mod-tab focus order is not very closely
-- related to the layout order. This is because we're forced to keep track of
-- the window positions separately, and this is ugly.  If you don't like this,
-- lobby for hierarchical stacks in core xmonad or go reim:lement the core of
-- xmonad yourself.

-- %import XMonad.Layout.Combo
-- %layout , combineTwo (TwoPane 0.03 0.5) (tabbed shrinkText defaultTConf) (tabbed shrinkText defaultTConf)

data CombineTwo l l1 l2 a = C2 [a] [a] l (l1 a) (l2 a)
                            deriving (Read, Show)

combineTwo :: (Read a, Eq a, LayoutClass super (), LayoutClass l1 a, LayoutClass l2 a) =>
              super () -> l1 a -> l2 a -> CombineTwo (super ()) l1 l2 a
combineTwo = C2 [] []

instance (LayoutClass l (), LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a)
    => LayoutClass (CombineTwo (l ()) l1 l2) a where
    doLayout (C2 f w2 super l1 l2) rinput s = arrange (integrate s)
        where arrange [] = do l1' <- maybe l1 id `fmap` handleMessage l1 (SomeMessage Hide)
                              l2' <- maybe l2 id `fmap` handleMessage l2 (SomeMessage Hide)
                              return ([], Just $ C2 [] [] super l1' l2')
              arrange [w] = do l1' <- maybe l1 id `fmap` handleMessage l1 (SomeMessage Hide)
                               l2' <- maybe l2 id `fmap` handleMessage l2 (SomeMessage Hide)
                               return ([(w,rinput)], Just $ C2 [w] [w] super l1' l2')
              arrange origws =
                  do let w2' = case origws `intersect` w2 of [] -> [head origws]
                                                             [x] -> [x]
                                                             x -> case origws \\ x of
                                                                  [] -> init x
                                                                  _ -> x
                         superstack = if focus s `elem` w2'
                                      then Stack { focus=(), up=[], down=[()] }
                                      else Stack { focus=(), up=[], down=[()] }
                         s1 = differentiate f' (origws \\ w2')
                         s2 = differentiate f' w2'
                         f' = focus s:delete (focus s) f
                     ([((),r1),((),r2)], msuper') <- doLayout super rinput superstack
                     (wrs1, ml1') <- runLayout l1 r1 s1
                     (wrs2, ml2') <- runLayout l2 r2 s2
                     return (wrs1++wrs2, Just $ C2 f' w2'
                                     (maybe super id msuper') (maybe l1 id ml1') (maybe l2 id ml2'))
    handleMessage (C2 f ws2 super l1 l2) m
        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `notElem` ws2,
          w2 `elem` ws2 = do l1' <- maybe l1 id `fmap` handleMessage l1 m
                             l2' <- maybe l2 id `fmap` handleMessage l2 m
                             return $ Just $ C2 f (w1:ws2) super l1' l2'
        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws2,
          w2 `notElem` ws2 = do l1' <- maybe l1 id `fmap` handleMessage l1 m
                                l2' <- maybe l2 id `fmap` handleMessage l2 m
                                let ws2' = case delete w1 ws2 of [] -> [w2]
                                                                 x -> x
                                return $ Just $ C2 f ws2' super l1' l2'
        | otherwise = do ml1' <- broadcastPrivate m [l1]
                         ml2' <- broadcastPrivate m [l2]
                         msuper' <- broadcastPrivate m [super]
                         if isJust msuper' || isJust ml1' || isJust ml2'
                            then return $ Just $ C2 f ws2
                                                 (maybe super head msuper')
                                                 (maybe l1 head ml1')
                                                 (maybe l2 head ml2')
                            else return Nothing
    description (C2 _ _ super l1 l2) = "combining "++ description l1 ++" and "++
                                       description l2 ++" with "++ description super


differentiate :: Eq q => [q] -> [q] -> Maybe (Stack q)
differentiate (z:zs) xs | z `elem` xs = Just $ Stack { focus=z
                                                     , up = reverse $ takeWhile (/=z) xs
                                                     , down = tail $ dropWhile (/=z) xs }
                        | otherwise = differentiate zs xs
differentiate [] xs = W.differentiate xs

broadcastPrivate :: LayoutClass l b => SomeMessage -> [l b] -> X (Maybe [l b])
broadcastPrivate a ol = do nml <- mapM f ol
                           if any isJust nml
                              then return $ Just $ zipWith ((flip maybe) id) ol nml
                              else return Nothing
    where f l = handleMessage l a `catchX` return Nothing
