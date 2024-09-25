{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Combo
-- Description :  A layout that combines multiple layouts.
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  none
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

import XMonad hiding (focus)
import XMonad.Layout.WindowNavigation (MoveWindowToWindow (..))
import XMonad.Prelude (delete, fromMaybe, intersect, isJust, (\\), listToMaybe)
import XMonad.StackSet (Stack (..), Workspace (..), integrate')
import XMonad.Util.Stack (zipperFocusedAtFirstOf)

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Layout.Combo
--
-- and add something like
--
-- > combineTwo (TwoPane 0.03 0.5) (tabbed shrinkText def) (tabbed shrinkText def)
--
-- to your layouts.
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".
--
-- combineTwo is a new simple layout combinator. It allows the
-- combination of two layouts using a third to split the screen
-- between the two, but has the advantage of allowing you to
-- dynamically adjust the layout, in terms of the number of windows in
-- each sublayout. To do this, use "XMonad.Layout.WindowNavigation",
-- and add the following key bindings (or something similar):
--
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
--
-- For detailed instruction on editing the key binding see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.
--
-- These bindings will move a window into the sublayout that is
-- up\/down\/left\/right of its current position.  Note that there is some
-- weirdness in combineTwo, in that the mod-tab focus order is not very closely
-- related to the layout order. This is because we're forced to keep track of
-- the window positions separately, and this is ugly.  If you don't like this,
-- lobby for hierarchical stacks in core xmonad or go reimplement the core of
-- xmonad yourself.

data CombineTwo l l1 l2 a = C2 [a] [a] l (l1 a) (l2 a)
                            deriving (Read, Show)

combineTwo :: (Read a, Eq a, LayoutClass super (), LayoutClass l1 a, LayoutClass l2 a) =>
              super () -> l1 a -> l2 a -> CombineTwo (super ()) l1 l2 a
combineTwo = C2 [] []

instance (LayoutClass l (), LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a)
    => LayoutClass (CombineTwo (l ()) l1 l2) a where
    runLayout (Workspace _ (C2 f w2 super l1 l2) s) rinput = arrange (integrate' s)
        where arrange [] = do l1' <- fromMaybe l1 <$> handleMessage l1 (SomeMessage ReleaseResources)
                              l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage ReleaseResources)
                              super' <- fromMaybe super <$>
                                        handleMessage super (SomeMessage ReleaseResources)
                              return ([], Just $ C2 [] [] super' l1' l2')
              arrange [w] = do l1' <- fromMaybe l1 <$> handleMessage l1 (SomeMessage ReleaseResources)
                               l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage ReleaseResources)
                               super' <- fromMaybe super <$>
                                         handleMessage super (SomeMessage ReleaseResources)
                               return ([(w,rinput)], Just $ C2 [w] [w] super' l1' l2')
              arrange origws =
                  do let w2' = case origws `intersect` w2 of [] -> take 1 origws
                                                             [x] -> [x]
                                                             x -> case origws \\ x of
                                                                  [] -> init x
                                                                  _ -> x
                         superstack = Stack { focus=(), up=[], down=[()] }
                         s1 = zipperFocusedAtFirstOf f' (origws \\ w2')
                         s2 = zipperFocusedAtFirstOf f' w2'
                         f' = case s of (Just s') -> focus s':delete (focus s') f
                                        Nothing -> f
                     ([((),r1),((),r2)], msuper') <- runLayout (Workspace "" super (Just superstack)) rinput
                     (wrs1, ml1') <- runLayout (Workspace "" l1 s1) r1
                     (wrs2, ml2') <- runLayout (Workspace "" l2 s2) r2
                     return (wrs1++wrs2, Just $ C2 f' w2'
                                     (fromMaybe super msuper') (fromMaybe l1 ml1') (fromMaybe l2 ml2'))
    handleMessage (C2 f ws2 super l1 l2) m
        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `notElem` ws2,
          w2 `elem` ws2 = do l1' <- fromMaybe l1 <$> handleMessage l1 m
                             l2' <- fromMaybe l2 <$> handleMessage l2 m
                             return $ Just $ C2 f (w1:ws2) super l1' l2'
        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws2,
          w2 `notElem` ws2 = do l1' <- fromMaybe l1 <$> handleMessage l1 m
                                l2' <- fromMaybe l2 <$> handleMessage l2 m
                                let ws2' = case delete w1 ws2 of [] -> [w2]
                                                                 x -> x
                                return $ Just $ C2 f ws2' super l1' l2'
        | otherwise = do ml1' <- broadcastPrivate m [l1]
                         ml2' <- broadcastPrivate m [l2]
                         msuper' <- broadcastPrivate m [super]
                         if isJust msuper' || isJust ml1' || isJust ml2'
                            then return $ Just $ C2 f ws2
                                                 (fromMaybe super (listToMaybe =<< msuper'))
                                                 (fromMaybe l1    (listToMaybe =<< ml1'))
                                                 (fromMaybe l2    (listToMaybe =<< ml2'))
                            else return Nothing
    description (C2 _ _ super l1 l2) = "combining "++ description l1 ++" and "++
                                       description l2 ++" with "++ description super

broadcastPrivate :: LayoutClass l b => SomeMessage -> [l b] -> X (Maybe [l b])
broadcastPrivate a ol = do nml <- mapM f ol
                           if any isJust nml
                              then return $ Just $ zipWith (`maybe` id) ol nml
                              else return Nothing
    where f l = handleMessage l a `catchX` return Nothing
