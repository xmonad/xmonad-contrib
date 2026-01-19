{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.ConditionalLayout
-- Copyright    : (c) Ivan Malison <IvanMalison@gmail.com>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- This module provides a LayoutModifier combinator that modifies an existing
-- ModifiedLayout so that its modifications are only applied when a particular
-- condition is met.
-----------------------------------------------------------------------------

module XMonad.Layout.ConditionalLayout where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

-- | A 'ModifierCondition' is a condition run in 'X' that takes a 'WorkspaceId'
-- as a parameter. The reason that this must exist as a type class and a simple
-- function will not suffice is that 'ModifierCondition's are used as parameters
-- to 'ConditionalLayoutModifier', which must implement 'Read' and 'Show' in
-- order to also implement 'LayoutModifier'. By defining a new type for
-- condition, we sidestep the issue that functions can not implement these
-- typeclasses.
class (Read c, Show c) => ModifierCondition c where
  shouldApply :: c -> WorkspaceId -> X Bool

-- | 'ConditionalLayoutModifier' takes a condition implemented as a
-- 'ModifierCondition' together with a 'LayoutModifier' and builds a new
-- 'LayoutModifier' that is exactly like the provided 'LayoutModifier', except
-- that it is only applied when the provided condition evalutes to True.
data ConditionalLayoutModifier m c a = (Read (m a), Show (m a), ModifierCondition c) =>
  ConditionalLayoutModifier c (m a)

deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Show (ConditionalLayoutModifier m c a)
deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Read (ConditionalLayoutModifier m c a)

data NoOpModifier a = NoOpModifier deriving (Read, Show)

instance LayoutModifier NoOpModifier a

instance (ModifierCondition c, LayoutModifier m Window) =>
  LayoutModifier (ConditionalLayoutModifier m c) Window where

  modifyLayout (ConditionalLayoutModifier condition originalModifier) w r = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then modifyLayout originalModifier w r
      else modifyLayout NoOpModifier w r

  modifyLayoutWithUpdate (ConditionalLayoutModifier condition originalModifier) w r = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then do
        (res, updatedModifier) <- modifyLayoutWithUpdate originalModifier w r
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> modifyLayoutWithUpdate NoOpModifier w r

  -- This function is not allowed to have any effect on layout, so we always
  -- pass the message along to the original modifier to ensure that it is
  -- allowed to update its internal state appropriately. This is particularly
  -- important for messages like 'Hide' or 'ReleaseResources'.
  handleMessOrMaybeModifyIt
    (ConditionalLayoutModifier condition originalModifier) mess = do
      result <- handleMessOrMaybeModifyIt originalModifier mess
      return $ case result of
                 Nothing -> Nothing
                 Just (Left updated) ->
                   Just $ Left $
                        ConditionalLayoutModifier condition updated
                 Just (Right message) -> Just $ Right message

  redoLayoutWithWorkspace (ConditionalLayoutModifier condition originalModifier)
                       w r ms wrs = do
    applyModifier <- shouldApply condition $ W.tag w
    if applyModifier
      then do
        (res, updatedModifier) <- redoLayout originalModifier r ms wrs
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> redoLayout NoOpModifier r ms wrs

  modifyDescription (ConditionalLayoutModifier _ originalModifier) l =
    modifyDescription originalModifier l


