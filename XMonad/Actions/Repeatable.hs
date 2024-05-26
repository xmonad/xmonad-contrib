{-# LANGUAGE LambdaCase #-}
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
-- "XMonad.Actions.CycleWorkspaceByScreen", "XMonad.Actions.CycleWindows" and
-- "XMonad.Actions.MostRecentlyUsed".
--
-- See the source of these modules for usage examples.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Repeatable
  ( repeatable
  , repeatableSt
  , repeatableM
  ) where

import Data.Bits

-- mtl
import Control.Monad.State (StateT(..))

-- X11
import Graphics.X11.Xlib.Extras

-- xmonad
import XMonad
import XMonad.Prelude


-- | An action that temporarily usurps and responds to key press/release events,
--   concluding when one of the modifier keys is released.
repeatable
  :: [KeySym]                      -- ^ The list of 'KeySym's under the
                                   --   modifiers used to invoke the action.
                                   --   If empty, auto-detect from
                                   --   'currentEvent'.
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
                                           --   If empty, auto-detect from
                                           --   'currentEvent'.
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
                                  --   If empty, auto-detect from
                                  --   'currentEvent'.
  -> KeySym                       -- ^ The keypress that invokes the action.
  -> (EventType -> KeySym -> m a) -- ^ The keypress handler.
  -> X b
repeatableM run mods key pressHandler = do
  XConf{ theRoot = root, display = d } <- ask
  mods' <- if null mods then getCurrentMods d else pure mods
  run (repeatableRaw d root mods' key pressHandler)

repeatableRaw
  :: (MonadIO m, Monoid a)
  => Display -> Window
  -> [KeySym] -> KeySym -> (EventType -> KeySym -> m a) -> m a
repeatableRaw d root mods key pressHandler
    | null mods = error "XMonad.Actions.Repeatable: null mods, would loop indefinitely"
    | otherwise = do
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

-- | Get 'KeySym's of currently pressed modifiers (assuming the event
-- currently being handled is a 'KeyEvent').
getCurrentMods :: Display -> X [KeySym]
getCurrentMods d = ask >>= \case
  XConf{ currentEvent = Just KeyEvent{ ev_state = mask } } -> io $ getCurrentMods' mask
  _ -> pure []
  where
    getCurrentMods' mask = do
      modMap <- modsToMasks <$> getModifierMapping d
      keycodesToKeysyms $ currentModKeys mask modMap

    modsToMasks :: [(Modifier, [KeyCode])] -> [(KeyMask, [KeyCode])]
    modsToMasks modMap = [ (mask, kcs) | (modi, kcs) <- modMap, mask <- maybeToList (modi `lookup` masks) ]

    masks =
      [ (shiftMapIndex,   shiftMask)
      , (lockMapIndex,    lockMask)
      , (controlMapIndex, controlMask)
      , (mod1MapIndex,    mod1Mask)
      , (mod2MapIndex,    mod2Mask)
      , (mod3MapIndex,    mod3Mask)
      , (mod4MapIndex,    mod4Mask)
      , (mod5MapIndex,    mod5Mask)
      ]

    currentModKeys :: KeyMask -> [(KeyMask, [KeyCode])] -> [KeyCode]
    currentModKeys mask modMap = [ kc | (m, kcs) <- modMap, mask .&. m /= 0, kc <- kcs, kc /= 0 ]

    keycodesToKeysyms :: [KeyCode] -> IO [KeySym]
    keycodesToKeysyms = traverse $ \kc -> keycodeToKeysym d kc 0
