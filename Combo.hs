{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Combo
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

module XMonadContrib.Combo (
                            -- * Usage
                            -- $usage 
                            combo, combineTwo
                           ) where

import Control.Arrow ( first )
import Data.List ( delete, intersect, (\\) )
import Data.Maybe ( isJust )
import XMonad
import Operations ( LayoutMessages(ReleaseResources,Hide) )
import StackSet ( integrate, Stack(..) )
import XMonadContrib.Invisible
import XMonadContrib.WindowNavigation ( MoveWindowToWindow(..) )
import qualified StackSet as W ( differentiate )

-- $usage
--
-- To use this layout write, in your Config.hs:
-- 
-- > import XMonadContrib.Combo 
-- 
-- and add something like
-- 
-- > combo (TwoPane 0.03 0.5) [(Layout Full,1),(Layout $ tabbed shrinkText defaultTConf,1)]
--
-- or alternatively
--
-- > combineTwo (TwoPane 0.03 0.5) (tabbed shrinkText defaultTConf) (tabbed shrinkText defaultTConf)
--
-- to your layouts.
--
-- The first argument to combo is a layout that will divide the screen into
-- one or more subscreens.  The second argument is a list of layouts which
-- will be used to lay out the contents of each of those subscreens.
-- Paired with each of these layouts is an integer giving the number of
-- windows this section should hold.  This number is ignored for the last
-- layout, which will hold any excess windows.

-- combineTwo is a new simpler (and yet in some ways more powerful) layout
-- combinator.  It only allows the combination of two layouts, but has the
-- advantage of allowing you to dynamically adjust the layout, in terms of
-- the number of windows in each sublayout.  To do this, use
-- WindowNavigation, and add the following key bindings (or something similar):

--     , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
--     , ((modMask .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)

-- These bindings will move a window into the sublayout that is
-- up/down/left/right of its current position.  Note that there is some
-- weirdness in combineTwo, in that the mod-tab focus order is not very
-- closely related to the layout order.  This is because we're forced to
-- keep track of the window positions sparately, and this is ugly.  If you
-- don't like this, lobby for hierarchical stacks in core xmonad or go
-- reimelement the core of xmonad yourself.

-- %import XMonadContrib.Combo
-- %layout , combo (twoPane 0.03 0.5) [(full,1),(tabbed shrinkText defaultTConf,1)]

data CombineTwo l l1 l2 a = C2 [a] [a] (l ()) (l1 a) (l2 a)
                            deriving (Read, Show)

combineTwo :: (Read a, Eq a, LayoutClass super (), LayoutClass l1 a, LayoutClass l2 a) =>
              super () -> l1 a -> l2 a -> CombineTwo super l1 l2 a
combineTwo = C2 [] []

instance (LayoutClass l (), LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a)
    => LayoutClass (CombineTwo l l1 l2) a where
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

combo :: (Eq a, Show a, Read a, ReadableLayout a, LayoutClass l (Layout a, Int))
         => (l (Layout a, Int)) -> [(Layout a, Int)] -> Combo l a
combo = Combo (I [])

data Combo l a = Combo (Invisible [] a) (l (Layout a, Int)) [(Layout a, Int)]
                 deriving (Show, Read)

instance (Eq a, ReadableLayout a, LayoutClass l (Layout a, Int))
    => LayoutClass (Combo l) a where
    doLayout (Combo (I f) super origls) rinput s = arrange (integrate s)
        where arrange [] = return ([], Just $ Combo (I []) super origls)
              arrange [w] = return ([(w,rinput)], Just $ Combo (I [w]) super origls)
              arrange origws =
                do (lrs, msuper') <- runLayout super rinput (W.differentiate $ take (length origws) origls)
                   let super' = maybe super id msuper'
                       f' = focus s:delete (focus s) f
                       lwrs [] _ = []
                       lwrs [((l,_),r)] ws = [((l,r),differentiate f' ws)]
                       lwrs (((l,n),r):xs) ws = ((l,r),differentiate f' $ take len1 ws) : lwrs xs (drop len1 ws)
                           where len1 = min n (length ws - length xs)
                   out <- mapM (uncurry $ uncurry runLayout) $ lwrs lrs origws
                   let origls' = zipWith foo (out++repeat ([],Nothing)) origls
                       foo (_, Nothing) x = x
                       foo (_, Just l') (_, n) = (l', n)
                   return (concat $ map fst out, Just $ Combo (I f') super' origls')
    handleMessage (Combo (I f) super origls) m =
                      do mls <- broadcastPrivate m (map fst origls)
                         let mls' = (\x->zipWith first (map const x) origls) `fmap` mls
                             f' = case fromMessage m of
                                  Just ReleaseResources -> []
                                  _ -> f
                         msuper <- broadcastPrivate m [super]
                         case msuper of
                           Just [super'] -> return $ Just $ Combo (I f') super' $ maybe origls id mls'
                           _ -> return $ Combo (I f') super `fmap` mls'

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
