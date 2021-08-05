{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.ConditionModifier
-- Copyright    : (c) Ivan Malison <IvanMalison@gmail.com>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- This module provides a LayoutModifier that modifies an existing
-- ModifiedLayout so that its modifications are only applied when a particular
-- condition is met.
-----------------------------------------------------------------------------

module XMonad.Layout.ConditionalModifier where

import XMonad
import XMonad.Layout.LayoutModifier

class (Read c, Show c) => ModifierCondition c where
  shouldApply :: c -> X Bool

data ConditionalLayoutModifier m c a = (Read (m a), Show (m a), ModifierCondition c) =>
  ConditionalLayoutModifier c (m a)

deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Show (ConditionalLayoutModifier m c a)
deriving instance (Read (m a), Show (m a), ModifierCondition c) =>
         Read (ConditionalLayoutModifier m c a)

data NoOpModifier a = NoOpModifier deriving (Read,Show)

instance LayoutModifier NoOpModifier a

runModifierIfCondition ::
  (ModifierCondition c, LayoutModifier m a) =>
  m a -> c -> (forall m1. LayoutModifier m1 a => m1 a -> X b) -> X b
runModifierIfCondition modifier condition action = do
  applyModifier <- shouldApply condition
  if applyModifier
    then action modifier
    else action NoOpModifier

instance (ModifierCondition c, LayoutModifier m Window) =>
  LayoutModifier (ConditionalLayoutModifier m c) Window where

  modifyLayout (ConditionalLayoutModifier condition originalModifier) w r =
    runModifierIfCondition originalModifier condition
                             (\modifier -> modifyLayout modifier w r)

  modifyLayoutWithUpdate (ConditionalLayoutModifier condition originalModifier) w r = do
    applyModifier <- shouldApply condition
    if applyModifier
      then do
        (res, updatedModifier) <- modifyLayoutWithUpdate originalModifier w r
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> modifyLayoutWithUpdate NoOpModifier w r

  -- This function is not allowed to have any downstream effect, so it seems
  -- more reasonable to simply allow the message to pass than to make it depend
  -- on the condition.
  handleMess (ConditionalLayoutModifier condition originalModifier) mess = do
    fmap (ConditionalLayoutModifier condition) <$> handleMess originalModifier mess

  handleMessOrMaybeModifyIt (ConditionalLayoutModifier condition originalModifier) mess = do
    applyModifier <- shouldApply condition
    if applyModifier
       then do
         result <- handleMessOrMaybeModifyIt originalModifier mess
         return $ case result of
           Nothing -> Nothing
           Just (Left updated) -> Just $ Left $ ConditionalLayoutModifier condition updated
           Just (Right message) -> Just $ Right message
       else return Nothing

  redoLayout (ConditionalLayoutModifier condition originalModifier) r ms wrs = do
    applyModifier <- shouldApply condition
    if applyModifier
      then do
        (res, updatedModifier) <- redoLayout originalModifier r ms wrs
        let updatedModifiedModifier =
              ConditionalLayoutModifier condition <$> updatedModifier
        return (res, updatedModifiedModifier)
      else (, Nothing) . fst <$> redoLayout NoOpModifier r ms wrs

  modifyDescription (ConditionalLayoutModifier _ originalModifier) l =
    modifyDescription originalModifier l


