{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{- |

Module      :  XMonad.Layout.FocusTracking
Description :  Track focus in the tiled layer.
Copyright   :  (c) 2010 & 2013 Adam Vogt
                   2011 Willem Vanlint
                   2018 & 2022 L.S.Leary
License     :  BSD-style (see xmonad/LICENSE)

Maintainer  :  @LSLeary (on github)
Stability   :  unstable
Portability :  unportable

FocusTracking simply holds onto the last true focus it was given and continues
to use it as the focus for the transformed layout until it sees another. It can
be used to improve the behaviour of a child layout that has not been given the
focused window, or equivalently, that of the layout itself when a float has
focus.

Relevant issues:

  * <http://code.google.com/p/xmonad/issues/detail?id=4>
  * <http://code.google.com/p/xmonad/issues/detail?id=306>

--------------------------------------------------------------------------------
-}
module XMonad.Layout.FocusTracking
    ( -- * Usage
      -- $usage
      FocusTracking(..)
    , focusTracking
    ) where

import XMonad.Prelude
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.Stack (findZ)
import qualified XMonad.StackSet as W

-- $usage
--
-- To use the module, first import it:
--
-- > import XMonad.Layout.FocusTracking
--
-- Then, a focus-dependent layout can be made to fall back on the last focus it
-- saw, for example:
--
-- > main = xmonad def
-- >  { layoutHook = someParentLayoutWith aChild (focusTracking anotherChild)
-- >  , ...
-- >  }
--
-- Or in a simpler case:
--
-- > main = xmonad def
-- >  { layoutHook = myTiledLayout ||| focusTracking Full
-- >  , ...
-- >  }
--

-- | A 'LayoutModifier' that remembers the last focus it saw.
newtype FocusTracking a = FocusTracking (Maybe Window)
    deriving (Read, Show)

instance LayoutModifier FocusTracking Window where
    modifyLayoutWithUpdate (FocusTracking mw) ws@W.Workspace{ W.stack = ms } r
      = do
        xCur <- gets (W.peek . W.view (W.tag ws) . windowset)
        let isF = xCur /= (W.focus <$> ms)
            -- use the remembered focus point when true focus differs from
            -- what this (sub)layout is given, which happens e.g. when true
            -- focus is in floating layer or when another sublayout has focus
            newStack | isF = (mw >>= \w -> findZ (w==) ms) <|> ms
                     | otherwise = ms
            newState | isF = mw
                     | otherwise = xCur
        ran <- runLayout ws{ W.stack = newStack } r
        return (ran, guard (newState /= mw) $> FocusTracking newState)

-- | Transform a layout into one that remembers and uses the last focus it saw.
focusTracking ::  l a -> ModifiedLayout FocusTracking l a
focusTracking = ModifiedLayout (FocusTracking Nothing)

