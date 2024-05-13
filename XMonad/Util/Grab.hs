{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Grab
-- Description :  Utilities for grabbing/ungrabbing keys.
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module should not be directly used by users. Its purpose is to
-- facilitate grabbing and ungrabbing keys.
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

module XMonad.Util.Grab
  (
 -- * Usage
 -- $Usage
    grabKP
  , ungrabKP
  , grabUngrab
  , grab
  , customRegrabEvHook
  ) where

-- core
import           XMonad

import           Control.Monad                  ( when )
import           Data.Foldable                  ( traverse_ )
-- base
import           Data.Semigroup                 ( All(..) )

-- }}}

-- --< Usage >-- {{{

-- $Usage
--
-- This module should not be directly used by users. Its purpose is to
-- facilitate grabbing and ungrabbing keys.

-- }}}

-- --< Public Utils >-- {{{

-- | A more convenient version of 'grabKey'.
grabKP :: KeyMask -> KeyCode -> X ()
grabKP mdfr kc = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (grabKey dpy kc mdfr rootw True grabModeAsync grabModeAsync)

-- | A more convenient version of 'ungrabKey'.
ungrabKP :: KeyMask -> KeyCode -> X ()
ungrabKP mdfr kc = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (ungrabKey dpy kc mdfr rootw)

-- | A convenience function to grab and ungrab keys
grabUngrab
  :: [(KeyMask, KeySym)] -- ^  Keys to grab
  -> [(KeyMask, KeySym)] -- ^ Keys to ungrab
  -> X ()
grabUngrab gr ugr = do
  traverse_ (uncurry ungrabKP) =<< mkGrabs ugr
  traverse_ (uncurry grabKP)   =<< mkGrabs gr

-- | A convenience function to grab keys. This also ungrabs all
-- previously grabbed keys.
grab :: [(KeyMask, KeySym)] -> X ()
grab ks = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io (ungrabKey dpy anyKey anyModifier rootw)
  grabUngrab ks []

-- | An event hook that runs a custom action to regrab the necessary keys.
customRegrabEvHook :: X () -> Event -> X All
customRegrabEvHook regr = \case
  e@MappingNotifyEvent{} -> do
    io (refreshKeyboardMapping e)
    when (ev_request e `elem` [mappingKeyboard, mappingModifier])
      $  cacheNumlockMask
      >> regr
    pure (All False)
  _ -> pure (All True)

-- }}}
