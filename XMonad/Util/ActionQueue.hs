{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ActionQueue
-- Description :  Queue of XMonad actions
-- Copyright   :  (c) 2021 Xiaokui Shu
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  subbyte@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Put XMonad actions in the queue to be executed in either the
-- @logHook@ or another hook of your choice.
-----------------------------------------------------------------------------

module XMonad.Util.ActionQueue ( -- * Usage
                                 -- $usage
                                 ActionQueue
                               , actionQueue
                               , enqueue
                               , exequeue
                               ) where

import XMonad
import qualified XMonad.Util.ExtensibleConf  as XC
import qualified XMonad.Util.ExtensibleState as XS

import Data.Sequence (Seq (..), ViewL (..), viewl, (|>))

-- $usage
--
-- This module provides a queue that, by default, gets executed every
-- time the @logHook@ runs.  To use this module
--
-- 1. Enqueue `X ()` actions at the place you need; e.g.:
--
-- > enqueue myAction
--
-- 2. Add the 'actionQueue' combinator to your configuration:
--
-- > main = xmonad $ actionQueue $ def
-- >     { ... }
--
-- This will execute all of the actions in the queue (if any) every time
-- the @logHook@ runs.  Developers of other extensions using this module
-- should re-export 'actionQueue'.
--
-- Alternatively, you can directly add 'exequeue' to a hook of your choice.
-- This is discouraged when writing user-facing modules, as (accidentally)
-- adding 'exequeue' to two different hooks might lead to undesirable
-- behaviour.  'actionQueue' uses the "XMonad.Util.ExtensibleConf" interface to
-- circumvent this.
--

newtype ActionQueue = ActionQueue (Seq (X ()))

instance ExtensionClass ActionQueue where
    initialValue = ActionQueue mempty

newtype ActionQueueHooked = ActionQueueHooked ()
  deriving newtype (Semigroup)

-- | Every time the @logHook@ runs, execute all actions in the queue.
actionQueue :: XConfig l -> XConfig l
actionQueue = XC.once (\cfg -> cfg{ logHook = logHook cfg <> exequeue })
                      ActionQueueHooked

-- | Enqueue an action.
enqueue :: X () -> X ()
enqueue = XS.modify . go
  where
    go :: X () -> ActionQueue -> ActionQueue
    go a (ActionQueue as) = ActionQueue $ as |> a

-- | Execute every action in the queue.
exequeue :: X ()
exequeue = do
    -- Note that we are executing all actions one by one.  Otherwise, we may
    -- not execute the actions in the right order.  Any of them may call
    -- 'refresh' or 'windows', which triggers the logHook, which may trigger
    -- 'exequeue' again if it is used in the logHook.
    ActionQueue aas <- XS.get
    case viewl aas of
      EmptyL  -> pure ()
      a :< as -> do XS.put (ActionQueue as)
                    a `catchX` pure ()
                    exequeue
