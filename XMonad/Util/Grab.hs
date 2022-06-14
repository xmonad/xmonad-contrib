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
import           Data.Bits                      ( setBit )
import           Data.Foldable                  ( traverse_ )
-- base
import qualified Data.Map.Strict               as M
import           Data.Semigroup                 ( All(..) )
import           Data.Traversable               ( for )

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
  f <- mkGrabs
  traverse_ (uncurry ungrabKP) (f ugr)
  traverse_ (uncurry grabKP)   (f gr)

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
      $  setNumlockMask
      >> regr
    pure (All False)
  _ -> pure (All True)

-- }}}

-- --< Private Utils >-- {{{

-- | Private action shamelessly copied and restyled from XMonad.Main source.
setNumlockMask :: X ()
setNumlockMask = withDisplay $ \dpy -> do
  ms <- io (getModifierMapping dpy)
  xs <- sequence
    [ do
        ks <- io (keycodeToKeysym dpy kc 0)
        pure $ if ks == xK_Num_Lock
          then setBit 0 (fromIntegral m)
          else 0 :: KeyMask
    | (m, kcs) <- ms
    , kc       <- kcs
    , kc /= 0
    ]
  modify $ \s -> s { numberlockMask = foldr (.|.) 0 xs }

-- | Private function shamelessly copied and refactored from XMonad.Main source.
mkGrabs :: X ([(KeyMask, KeySym)] -> [(KeyMask, KeyCode)])
mkGrabs = withDisplay $ \dpy -> do
  let (minCode, maxCode) = displayKeycodes dpy
      allCodes           = [fromIntegral minCode .. fromIntegral maxCode]
  syms <- io . for allCodes $ \code -> keycodeToKeysym dpy code 0
  let keysymMap = M.fromListWith (++) (zip syms $ pure <$> allCodes)
      keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
  extraMods <- extraModifiers
  pure $ \ks -> do
    (mask, sym) <- ks
    keycode     <- keysymToKeycodes sym
    extraMod    <- extraMods
    pure (mask .|. extraMod, keycode)

-- }}}


-- NOTE: there is some duplication between this module and core. The
-- latter probably will never change, but this needs to be kept in sync
-- with any potential bugs that might arise.
