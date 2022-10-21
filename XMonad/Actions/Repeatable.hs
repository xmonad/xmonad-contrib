-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Repeatable
-- Description :  Actions you'd like to repeat.
-- Copyright   :  (c) 2022 L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  @LSLeary (on github)
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module factors out the shared logic of "XMonad.Actions.CycleRecentWS",
-- "XMonad.Actions.CycleWorkspaceByScreen" and "XMonad.Actions.CycleWindows".
--
-- See the source of these modules for usage examples.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Repeatable
  ( repeatable
  , repeatableSt
  , repeatableM
  ) where

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
repeatableSt iSt = repeatableM $ \m -> runStateT m iSt

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
repeatableM run mods key pressHandler = do
  XConf{ theRoot = root, display = d } <- ask
  run (repeatableRaw d root mods key pressHandler)

repeatableRaw
  :: (MonadIO m, Monoid a)
  => Display -> Window
  -> [KeySym] -> KeySym -> (EventType -> KeySym -> m a) -> m a
repeatableRaw d root mods key pressHandler = do
  io (grabKeyboard d root False grabModeAsync grabModeAsync currentTime)
  handleEvent (keyPress, key) <* io (ungrabKeyboard d currentTime)
  where
    getNextEvent = io $ allocaXEvent $ \p -> do
      maskEvent d (keyPressMask .|. keyReleaseMask) p
      KeyEvent{ ev_event_type = t, ev_keycode = c } <- getEvent p
      s <- keycodeToKeysym d c 0
      return (t, s)
    handleEvent (t, s)
      | t == keyRelease && s `elem` mods = pure mempty
      | otherwise = (<>) <$> pressHandler t s <*> (getNextEvent >>= handleEvent)
