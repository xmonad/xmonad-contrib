{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IM
-- Description :  Layout modfier for multi-windowed instant messengers like Psi or Tkabber.
-- Copyright   :  (c) Roman Cheplyaka, Ivan N. Veselov <veselov@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modfier suitable for workspace with multi-windowed instant messenger
-- (like Psi or Tkabber).
--
-----------------------------------------------------------------------------

module XMonad.Layout.IM (
    -- * Usage
    -- $usage

    -- * Hints
    -- $hints

    -- * TODO
    -- $todo
        Property(..), IM(..), withIM, gridIM,
        AddRoster,
) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties

import Control.Arrow (first)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.IM
-- > import Data.Ratio ((%))
--
-- Then edit your @layoutHook@ by adding IM modifier to layout which you prefer
-- for managing your chat windows (Grid in this example, another useful choice
-- to consider is Tabbed layout).
--
-- > myLayout = withIM (1%7) (ClassName "Tkabber") Grid ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- Here @1%7@ is the part of the screen which your roster will occupy,
-- @ClassName \"Tkabber\"@ tells xmonad which window is actually your roster.
--
-- Screenshot: <http://haskell.org/haskellwiki/Image:Xmonad-layout-im.png>
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- $hints
--
-- To launch IM layout automatically on your IM workspace use "XMonad.Layout.PerWorkspace".
--
-- By default the roster window will appear on the left side.
-- To place roster window on the right side, use @reflectHoriz@ from
-- "XMonad.Layout.Reflect" module.

-- $todo
-- This item are questionable. Please let me know if you find them useful.
--
-- * shrink\/expand
--

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRoster a = AddRoster Rational Property deriving (Read, Show)

instance LayoutModifier AddRoster Window where
  modifyLayout (AddRoster ratio prop) = applyIM ratio prop
  modifierDescription _                = "IM"

-- | Modifier which converts given layout to IM-layout (with dedicated
-- space for roster and original layout for chat windows)
withIM :: LayoutClass l a => Rational -> Property -> l a -> ModifiedLayout AddRoster l a
withIM ratio prop = ModifiedLayout $ AddRoster ratio prop

-- | IM layout modifier applied to the Grid layout
gridIM :: Rational -> Property -> ModifiedLayout AddRoster Grid a
gridIM ratio prop = withIM ratio prop Grid

-- | Internal function for adding space for the roster specified by
-- the property and running original layout for all chat windows
applyIM :: (LayoutClass l Window) =>
               Rational
            -> Property
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIM ratio prop wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' stack
    let (masterRect, slaveRect) = splitHorizontallyBy ratio rect
    master <- findM (hasProperty prop) ws
    case master of
        Just w -> do
            let filteredStack = stack >>= S.filter (w /=)
            wrs <- runLayout (wksp {S.stack = filteredStack}) slaveRect
            return (first ((w, masterRect) :) wrs)
        Nothing -> runLayout wksp rect

-- | Like find, but works with monadic computation instead of pure function.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do { b <- f x; if b then return (Just x) else findM f xs }

-- | This is for compatibility with old configs only and will be removed in future versions!
data IM a = IM Rational Property deriving (Read, Show)
instance LayoutClass IM Window where
    description _ = "IM"
    doLayout (IM r prop) rect stack = do
        let ws = S.integrate stack
        let (masterRect, slaveRect) = splitHorizontallyBy r rect
        master <- findM (hasProperty prop) ws
        let positions = case master of
                Just w -> (w, masterRect) : arrange defaultRatio slaveRect (filter (w /=) ws)
                Nothing -> arrange defaultRatio rect ws
        return (positions, Nothing)
