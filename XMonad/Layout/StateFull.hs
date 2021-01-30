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

module XMonad.Layout.StateFull (
  -- * Usage
  -- $Usage
  pattern StateFull,
  StateFull,
  FocusTracking(..),
  focusTracking
) where

import XMonad hiding ((<&&>))
import qualified XMonad.StackSet as W
import XMonad.Util.Stack (findZ)

import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

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

-- | The @FocusTracking@ data type for which the @LayoutClass@ instance is
--   provided.
data FocusTracking l a = FocusTracking (Maybe a) (l a)
  deriving (Show, Read)

-- | Transform a layout into one that remembers and uses its last focus.
focusTracking :: l a -> FocusTracking l a
focusTracking = FocusTracking Nothing

-- | A type synonym to match the @StateFull@ pattern synonym.
type StateFull = FocusTracking Full

-- | A pattern synonym for the primary use case of the @FocusTracking@
--   transformer; using @Full@.
pattern StateFull = FocusTracking Nothing Full

instance LayoutClass l Window => LayoutClass (FocusTracking l) Window where

  description (FocusTracking _ child)
    | (chDesc == "Full")  = "StateFull"
    | (' ' `elem` chDesc) = "FocusTracking (" ++ chDesc ++ ")"
    | otherwise           = "FocusTracking " ++ chDesc
    where chDesc = description child

  runLayout (W.Workspace i (FocusTracking mOldFoc childL) mSt) sr = do

    mRealFoc <- gets (W.peek . windowset)
    let mGivenFoc = W.focus <$> mSt
        passedMSt = if mRealFoc == mGivenFoc then mSt
                    else (mOldFoc >>= \oF -> findZ (==oF) mSt) <|> mSt

    (wrs, mChildL') <- runLayout (W.Workspace i childL passedMSt) sr
    let newFT = if mRealFoc /= mGivenFoc then FocusTracking mOldFoc <$> mChildL'
                else Just $ FocusTracking mGivenFoc (fromMaybe childL mChildL')

    return (wrs, newFT)

  handleMessage (FocusTracking mf childLayout) m =
    (fmap . fmap) (FocusTracking mf) (handleMessage childLayout m)
