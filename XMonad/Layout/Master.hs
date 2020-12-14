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

    mastered,
    fixMastered,
    multimastered,
    AddMaster,
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
-- Or if you prefer to have a master with fixed width:
--
-- > fixMastered (1/100) (1/2) $ Grid
--
-- Or if you want multiple (here two) master windows from the beginning:
--
-- > multimastered 2 (1/100) (1/2) $ Grid
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
data AddMaster a = AddMaster Int Rational Rational deriving (Show, Read)

multimastered :: (LayoutClass l a) =>
       Int -- ^ @k@, number of master windows
    -> Rational -- ^ @delta@, the ratio of the screen to resize by
    -> Rational -- ^ @frac@, what portion of the screen to use for the master window
    -> l a      -- ^ the layout to be modified
    -> ModifiedLayout AddMaster l a
multimastered k delta frac = ModifiedLayout $ AddMaster k delta frac

mastered :: (LayoutClass l a) =>
       Rational -- ^ @delta@, the ratio of the screen to resize by
    -> Rational -- ^ @frac@, what portion of the screen to use for the master window
    -> l a      -- ^ the layout to be modified
    -> ModifiedLayout AddMaster l a
mastered delta frac = multimastered 1 delta frac

instance LayoutModifier AddMaster Window where
    modifyLayout (AddMaster k delta frac) = applyMaster False k delta frac
    modifierDescription _               = "Mastered"

    pureMess (AddMaster k delta frac) m
        | Just Shrink <- fromMessage m = Just $ AddMaster k delta (frac-delta)
        | Just Expand <- fromMessage m = Just $ AddMaster k delta (frac+delta)
        | Just (IncMasterN d) <- fromMessage m = Just $ AddMaster (max 1 (k+d)) delta frac

    pureMess _ _ = Nothing

data FixMaster a = FixMaster (AddMaster a) deriving (Show, Read)

instance LayoutModifier FixMaster Window where
    modifyLayout (FixMaster (AddMaster k d f)) = applyMaster True k d f
    modifierDescription (FixMaster a) = "Fix" ++ modifierDescription a
    pureMess (FixMaster a) m = fmap FixMaster (pureMess a m)

fixMastered :: (LayoutClass l a) =>
       Rational -- ^ @delta@, the ratio of the screen to resize by
    -> Rational -- ^ @frac@, what portion of the screen to use for the master window
    -> l a      -- ^ the layout to be modified
    -> ModifiedLayout FixMaster l a
fixMastered delta frac = ModifiedLayout . FixMaster $ AddMaster 1 delta frac

-- | Internal function for adding a master window and let the modified
-- layout handle the rest of the windows
applyMaster :: (LayoutClass l Window) =>
                  Bool
               -> Int
               -> Rational
               -> Rational
               -> S.Workspace WorkspaceId (l Window) Window
               -> Rectangle
               -> X ([(Window, Rectangle)], Maybe (l Window))
applyMaster f k _ frac wksp rect = do
    let st= S.stack wksp
    let ws = S.integrate' $ st
    let n = length ws + fromEnum f
    if n > 1 then
        if(n<=k) then
             return ((divideCol rect ws), Nothing)
             else do
             let m = take k ws
             let (mr, sr) = splitHorizontallyBy frac rect
             let nst = st>>= S.filter (\w -> not (w `elem` m))
             wrs <- runLayout (wksp {S.stack = nst}) sr
             return ((divideCol mr m) ++ (fst wrs), snd wrs)
        else runLayout wksp rect

-- | Shift rectangle down
shiftD :: Position -> Rectangle -> Rectangle
shiftD s (Rectangle x y w h) = Rectangle x (y+s) w h

-- | Divide rectangle between windows
divideCol :: Rectangle -> [a] -> [(a, Rectangle)]
divideCol (Rectangle x y w h) ws = zip ws rects
    where n = length ws
          oneH = fromIntegral h `div` n
          oneRect = Rectangle x y w (fromIntegral oneH)
          rects = take n $ iterate (shiftD (fromIntegral oneH)) oneRect

