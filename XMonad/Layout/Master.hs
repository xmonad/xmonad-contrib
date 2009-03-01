{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Master
-- Copyright   :  (c) Ismael Carnales, Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Ismael Carnales <icarnales@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modfier that adds a master window to another layout.
-----------------------------------------------------------------------------

module XMonad.Layout.Master (
    -- * Usage
    -- $usage

    mastered
) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Master
--
-- Then edit your @layoutHook@ and add the Master modifier to the layout that
-- you prefer.
--
-- > mastered (1/100) (1/2) $ Grid
--
-- This will use the left half of your screen for a master window and let
-- Grid manage the right half.
--
-- For more detailed instructions on editing the layoutHook see
-- "XMonad.Doc.Extending#Editing_the_layout_hook".
--
-- Like 'XMonad.Layout.Tall', 'withMaster' supports the
-- 'XMonad.Layout.Shrink' and XMonad.Layout.Expand' messages.

-- | Data type for LayoutModifier which converts given layout to a mastered
-- layout
data AddMaster a = AddMaster Rational Rational deriving (Show, Read)

-- | Modifier wich converts given layout to a mastered one
mastered :: (LayoutClass l a) =>
       Rational -- ^ @delta@, the ratio of the screen to resize by
    -> Rational -- ^ @frac@, what portion of the screen to use for the master window
    -> l a      -- ^ the layout to be modified
    -> ModifiedLayout AddMaster l a
mastered delta frac = ModifiedLayout $ AddMaster delta frac

instance LayoutModifier AddMaster Window where
    modifyLayout (AddMaster delta frac) = applyMaster delta frac
    modifierDescription _               = "Mastered"

    pureMess (AddMaster delta frac) m
        | Just Shrink <- fromMessage m = Just $ AddMaster delta (frac-delta)
        | Just Expand <- fromMessage m = Just $ AddMaster delta (frac+delta)

    pureMess _ _ = Nothing

-- | Internal function for adding a master window and let the modified
-- layout handle the rest of the windows
applyMaster :: (LayoutClass l Window) =>
                  Rational
               -> Rational
               -> S.Workspace WorkspaceId (l Window) Window
               -> Rectangle
               -> X ([(Window, Rectangle)], Maybe (l Window))
applyMaster _ frac wksp rect = do
    let st= S.stack wksp
    let ws = S.integrate' $ st
    if length ws > 1 then do
        let m = head ws
        let (mr, sr) = splitHorizontallyBy frac rect
        let nst = st>>= S.filter (m/=)
        wrs <- runLayout (wksp {S.stack = nst}) sr
        return ((m, mr) : fst wrs, snd wrs)

        else runLayout wksp rect
