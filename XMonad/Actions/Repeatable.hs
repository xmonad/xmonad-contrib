{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Repeatable
-- Description :  Actions you'd like to repeat.
-- Copyright   :  (c) 2022,2026 L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L.S.Leary.II@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module factors out the shared logic of "XMonad.Actions.CycleRecentWS",
-- "XMonad.Actions.CycleWorkspaceByScreen", "XMonad.Actions.CycleWindows" and
-- "XMonad.Actions.MostRecentlyUsed".
--
-- See the source of these modules for usage examples.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Repeatable (

  -- * Repeatable
  repeatable,
  repeatableSt,
  repeatableM,

  -- * Concludable
  NotOurEvent(..),
  Done(..),
  concludable,
  concludableSt,
  concludableM,

) where

-- base
import Data.Functor (($>))

-- mtl
import Control.Monad.State (StateT(..))

-- X11
import Graphics.X11.Xlib.Extras

-- xmonad
import XMonad


-- | An action that temporarily usurps and responds to key press/release events,
--   concluding when one of the modifier keys is released.
repeatable
  :: [KeySym]                      -- ^ The list of 'KeySym's under the
                                   --   modifiers used to invoke the action.
  -> KeySym                        -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> X ()) -- ^ The keypress handler.
  -> X ()
repeatable = repeatableM id

-- | A more general variant of 'repeatable' with a stateful handler,
--   accumulating a monoidal return value throughout the events.
repeatableSt
  :: Monoid a
  => s                                     -- ^ Initial state.
  -> [KeySym]                              -- ^ The list of 'KeySym's under the
                                           --   modifiers used to invoke the
                                           --   action.
  -> KeySym                                -- ^ The keypress that invokes the
                                           --   action.
  -> (EventType -> KeySym -> StateT s X a) -- ^ The keypress handler.
  -> X (a, s)
repeatableSt iSt = repeatableM (`runStateT` iSt)

-- | A more general variant of 'repeatable' with an arbitrary monadic handler,
--   accumulating a monoidal return value throughout the events.
repeatableM
  :: (MonadIO m, Monoid a)
  => (m a -> X b)                 -- ^ How to run the monad in 'X'.
  -> [KeySym]                     -- ^ The list of 'KeySym's under the
                                  --   modifiers used to invoke the action.
  -> KeySym                       -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> m a) -- ^ The keypress handler.
  -> X b
repeatableM run mods key handler = concludableM run mods key press event
 where
  press t s = pure (Right (t, s))
  event (t, s) = Right <$> handler t s


data Done        = Done
data NotOurEvent = NotOurEvent

-- | A generalisation of `repeatable` which may conclude early with `NotOurEvent` or `Done`.
concludable
  :: [KeySym]
  -- ^ The list of 'KeySym's under the modifiers used to invoke the action.
  -> KeySym
  -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> IO (Either NotOurEvent e))
  -- ^ Handle keypresses by translating them into custom events.
  --   If the function produces `NotOurEvent` then we conclude and put the
  --   X `Event` back into the queue.
  -> (e -> X (Either Done ()))
  -- ^ The custom event handler.
  -> X ()
concludable = concludableM id

-- | A more general variant of 'concludable' with a stateful handler,
--   accumulating a monoidal return value throughout the events.
concludableSt
  :: Monoid a
  => s
  -- ^ Initial state.
  -> [KeySym]
  -- ^ The list of 'KeySym's under the modifiers used to invoke the action.
  -> KeySym
  -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> IO (Either NotOurEvent e))
  -- ^ Handle keypresses by translating them into custom events.
  --   If the function produces `NotOurEvent` then we conclude and put the
  --   X `Event` back into the queue.
  -> (e -> StateT s X (Either Done a))
  -- ^ The custom event handler.
  -> X (a, s)
concludableSt iSt = concludableM (`runStateT` iSt)

-- | A more general variant of 'concludable' with an arbitrary monadic handler,
--   accumulating a monoidal return value throughout the events.
concludableM
  :: (MonadIO m, Monoid a)
  => (m a -> X b)
  -- ^ How to run the monad in 'X'.
  -> [KeySym]
  -- ^ The list of 'KeySym's under the modifiers used to invoke the action.
  -> KeySym
  -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> IO (Either NotOurEvent e))
  -- ^ Handle keypresses by translating them into custom events.
  --   If the function produces `NotOurEvent` then we conclude and put the
  --   X `Event` back into the queue.
  -> (e -> m (Either Done a))
  -- ^ The custom event handler.
  -> X b
concludableM run mods key pressHandler eventHandler = do
  XConf{ theRoot = root, display = d } <- ask
  run (concludableRaw d root mods key pressHandler eventHandler)

concludableRaw
  :: (MonadIO m, Monoid a)
  => Display -> Window
  -> [KeySym] -> KeySym
  -> (EventType -> KeySym -> IO (Either NotOurEvent e))
  -> (e -> m (Either Done a))
  -> m a
concludableRaw d root mods key pressHandler eventHandler = do
  io (grabKeyboard d root False grabModeAsync grabModeAsync currentTime)
  mev <- io (pressHandler' (pure ()) keyPress key)
  x   <- maybe (pure mempty) (eventHandler' mempty) mev
  io (ungrabKeyboard d currentTime)
  pure x
 where
  pressHandler' putBack t s
    | t == keyRelease && s `elem` mods = pure Nothing
    | otherwise                        = pressHandler t s >>= \case
      Left NotOurEvent -> putBack $> Nothing
      Right ev         -> pure (Just ev)
  eventHandler' !x ev = do
    c <- eventHandler ev
    case c of
      Left  Done -> pure x
      Right y    -> do
        mev <- getNextEvent
        maybe (pure xy) (eventHandler' xy) mev
       where xy = x <> y
  getNextEvent = (io . allocaXEvent) \p -> do
    maskEvent d (keyPressMask .|. keyReleaseMask) p
    KeyEvent{ ev_event_type = t, ev_keycode = c } <- getEvent p
    s <- keycodeToKeysym d c 0
    pressHandler' (putBackEvent d p) t s
