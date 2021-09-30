{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ActionCycle
-- Description :  Provides a way to implement cycling actions.
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides a way to have "cycling" actions.
-- This means that you can define an @X ()@ action that cycles through a list of actions,
-- advancing every time it is executed.
-- This may for exapmle be useful for toggle-style keybindings.
--
-----------------------------------------------------------------------------

module XMonad.Util.ActionCycle
  ( -- * Usage
    -- $usage
    cycleAction
  , cycleActionWithResult
  )
where
import Prelude hiding ((!!))
import Data.Map.Strict as M
import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((!!), NonEmpty((:|)))


-- $usage
-- You can use this module to implement cycling key-bindings by importing 'XMonad.Util.ActionCycle'
--
-- > import XMonad.Util.ActionCycle
--
-- and then creating a keybinding as follows:
--
-- > ((mod1Mask, xK_t), cycleAction "cycleActions" [ spawn "commmand1", spawn "command2", spawn "command3" ])
--
-- Note that the name given to cycleAction must be a unique action per cycle.


-- | Generate an @X ()@ action that cycles through a list of actions,
-- advancing every time the action is called.
cycleAction
  :: String -- ^ Unique name for this action. May be any arbitrary, unique string.
  -> [X ()] -- ^ List of actions that will be cycled through.
  -> X ()
cycleAction _ [] = pure ()
cycleAction name (x:xs) = cycleActionWithResult name (x :| xs)

-- | Another version of 'cycleAction' that returns the result of the actions.
-- To allow for this, we must make sure that the list of actions is non-empty.
cycleActionWithResult
  :: String                   -- ^ Unique name for this action. May be any arbitrary, unique string.
  -> NonEmpty.NonEmpty (X a)  -- ^ Non-empty List of actions that will be cycled through.
  -> X a
cycleActionWithResult name actions = do
  cycleState <- XS.gets (getActionCycle name)
  idx <- case cycleState of
    Just x -> do
      XS.modify (nextActionCycle name (NonEmpty.length actions))
      pure x
    Nothing -> do
      XS.modify (setActionCycle name 1)
      pure 0
  actions !! idx


newtype ActionCycleState = ActionCycleState (M.Map String Int)

instance ExtensionClass ActionCycleState where
  initialValue = ActionCycleState mempty

getActionCycle :: String -> ActionCycleState -> Maybe Int
getActionCycle name (ActionCycleState s) = M.lookup name s

nextActionCycle :: String -> Int -> ActionCycleState -> ActionCycleState
nextActionCycle name maxNum (ActionCycleState s) = ActionCycleState $ M.update (\n -> Just $ (n + 1) `mod` maxNum) name s

setActionCycle :: String -> Int -> ActionCycleState -> ActionCycleState
setActionCycle name n (ActionCycleState s) = ActionCycleState $ M.insert name n s
