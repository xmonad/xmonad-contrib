{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutCombinators
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for combining Layouts
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutCombinators (
    -- * Usage
    -- $usage
    (<|>), (</>), (<||>), (<//>), (|||), JumpToLayout(JumpToLayout)
    ) where

import Data.Maybe ( isJust )

import XMonad
import Layouts ( Tall(..), Mirror(..), ChangeLayout(NextLayout) )
import XMonad.Layout.Combo
import XMonad.Layout.DragPane

-- $usage
-- Use LayoutCombinators to easily combine Layouts.

(<||>), (<//>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
                  l1 a -> l2 a -> CombineTwo DragPane l1 l2 a
(<|>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
         => l1 a -> l2 a -> CombineTwo Tall l1 l2 a
(</>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
         => l1 a -> l2 a -> CombineTwo (Mirror Tall) l1 l2 a

(<||>) = combineTwo (dragPane Vertical 0.1 0.5)
(<//>) = combineTwo (dragPane Horizontal 0.1 0.5)
(<|>) = combineTwo (Tall 1 0.1 0.5)
(</>) = combineTwo (Mirror $ Tall 1 0.1 0.5)

(|||) :: (LayoutClass l1 a, LayoutClass l2 a) => l1 a -> l2 a -> NewSelect l1 l2 a
(|||) = NewSelect True

data NewSelect l1 l2 a = NewSelect Bool (l1 a) (l2 a) deriving ( Read, Show )

data NoWrap = NextLayoutNoWrap | Wrap deriving ( Read, Show, Typeable )
instance Message NoWrap

data JumpToLayout = JumpToLayout String deriving ( Read, Show, Typeable )
instance Message JumpToLayout

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (NewSelect l1 l2) a where
    doLayout (NewSelect True l1 l2) r s = do (wrs, ml1') <- doLayout l1 r s
                                             return (wrs, (\l1' -> NewSelect True l1' l2) `fmap` ml1')
    doLayout (NewSelect False l1 l2) r s = do (wrs, ml2') <- doLayout l2 r s
                                              return (wrs, (\l2' -> NewSelect False l1 l2') `fmap` ml2')
    description (NewSelect True l1 _) = description l1
    description (NewSelect False _ l2) = description l2
    handleMessage (NewSelect False l1 l2) m
        | Just Wrap <- fromMessage m =
                       do ml2' <- handleMessage l2 (SomeMessage Hide)
                          ml1' <- handleMessage l1 m
                          return $ Just $ NewSelect True (maybe l1 id ml1') (maybe l2 id ml2')
    handleMessage (NewSelect True l1 l2) m
        | Just NextLayoutNoWrap <- fromMessage m =
                 do ml1' <- handleMessage l1 m
                    case ml1' of
                      Just l1' -> return $ Just (NewSelect True l1' l2)
                      Nothing -> do ml1'' <- handleMessage l1 (SomeMessage Hide)
                                    ml2' <- handleMessage l2 (SomeMessage Wrap)
                                    return $ Just $ NewSelect False (maybe l1 id ml1'') (maybe l2 id ml2')
    handleMessage l@(NewSelect True _ _) m
        | Just NextLayout <- fromMessage m = handleMessage l (SomeMessage NextLayoutNoWrap)
    handleMessage l@(NewSelect False l1 l2) m
        | Just NextLayout <- fromMessage m =
                 do ml' <- handleMessage l (SomeMessage NextLayoutNoWrap)
                    case ml' of
                      Just l' -> return $ Just l'
                      Nothing -> do ml2' <- handleMessage l2 (SomeMessage Hide)
                                    ml1' <- handleMessage l1 (SomeMessage Wrap)
                                    return $ Just $ NewSelect True (maybe l1 id ml1') (maybe l2 id ml2')
    handleMessage (NewSelect True l1 l2) m
        | Just (JumpToLayout d) <- fromMessage m,
          d == description l2 = do ml1' <- handleMessage l1 (SomeMessage Hide)
                                   return $ Just $ NewSelect False (maybe l1 id ml1') l2
    handleMessage (NewSelect True l1 l2) m
        | Just (JumpToLayout d) <- fromMessage m
        = do ml1' <- handleMessage l1 m
             case ml1' of
               Just l1' -> return $ Just $ NewSelect True l1' l2
               Nothing ->
                   do ml2' <- handleMessage l2 m
                      case ml2' of
                        Nothing -> return Nothing
                        Just l2' -> do ml1'' <- handleMessage l1 (SomeMessage Hide)
                                       return $ Just $ NewSelect False (maybe l1 id ml1'') l2'
    handleMessage (NewSelect False l1 l2) m
        | Just (JumpToLayout d) <- fromMessage m,
          d == description l1 = do ml2' <- handleMessage l2 (SomeMessage Hide)
                                   return $ Just $ NewSelect True l1 (maybe l2 id ml2')
    handleMessage (NewSelect False l1 l2) m
        | Just (JumpToLayout d) <- fromMessage m
        = do ml2' <- handleMessage l2 m
             case ml2' of
               Just l2' -> return $ Just $ NewSelect False l1 l2'
               Nothing ->
                   do ml1' <- handleMessage l1 m
                      case ml1' of
                        Nothing -> return Nothing
                        Just l1' -> do ml2'' <- handleMessage l2 (SomeMessage Hide)
                                       return $ Just $ NewSelect True l1' (maybe l2 id ml2'')
    handleMessage (NewSelect b l1 l2) m
        | Just ReleaseResources  <- fromMessage m =
        do ml1' <- handleMessage l1 m
           ml2' <- handleMessage l2 m
           return $ if isJust ml1' || isJust ml2'
                    then Just $ NewSelect b (maybe l1 id ml1') (maybe l2 id ml2')
                    else Nothing
    handleMessage (NewSelect True l1 l2) m =
        do ml1' <- handleMessage l1 m
           return $ (\l1' -> NewSelect True l1' l2) `fmap` ml1'
    handleMessage (NewSelect False l1 l2) m =
        do ml2' <- handleMessage l2 m
           return $ (\l2' -> NewSelect False l1 l2') `fmap` ml2'
