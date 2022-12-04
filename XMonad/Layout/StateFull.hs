{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.StateFull
-- Description :  The StateFull Layout & FocusTracking Layout Transformer
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides StateFull: a stateful form of Full that does not misbehave when
-- floats are focused, and the FocusTracking layout transformer by means of
-- which StateFull is implemented. FocusTracking simply holds onto the last
-- true focus it was given and continues to use it as the focus for the
-- transformed layout until it sees another. It can be used to improve the
-- behaviour of a child layout that has not been given the focused window.
--------------------------------------------------------------------------------

module XMonad.Layout.StateFull {-# DEPRECATED "Use X.L.TrackFloating." #-} (
  -- * Usage
  -- $Usage
  pattern StateFull,
  StateFull,
  FocusTracking,
  F.focusTracking
) where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.FocusTracking as F

-- $Usage
--
-- To use it, first you need to:
--
-- > import XMonad.Layout.StateFull
--
-- Then to toggle your tiled layout with @StateFull@, you can do:
--
-- > main = xmonad def { layoutHook = someTiledLayout ||| StateFull }
--
-- Or, some child layout that depends on focus information can be made to fall
-- back on the last focus it had:
--
-- > main = xmonad def
-- >  { layoutHook = someParentLayoutWith aChild (focusTracking anotherChild) }

-- | The @FocusTracking@ type for which the @LayoutClass@ instance is provided.
type FocusTracking = ModifiedLayout F.FocusTracking

-- | A type synonym to match the @StateFull@ pattern synonym.
type StateFull = FocusTracking Full

-- | A pattern synonym for the primary use case of the @FocusTracking@
--   transformer; using @Full@.
pattern StateFull :: StateFull a
pattern StateFull = ModifiedLayout (F.FocusTracking Nothing) Full

