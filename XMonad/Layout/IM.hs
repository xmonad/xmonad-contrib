{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IM
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout suitable for workspace with multi-windowed instant messanger (like
-- Psi or Tkabber).
--
-----------------------------------------------------------------------------

module XMonad.Layout.IM (
    -- * Usage
    -- $usage

    -- * Hints
    -- $hints

    -- * TODO
    -- $todo
        Property(..), IM(..)
) where

import XMonad
import qualified XMonad.StackSet as S
import Data.List
import XMonad.Layout (splitHorizontallyBy)
import XMonad.Layout.Grid (arrange)
import XMonad.Util.WindowProperties

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.IM
-- > import Data.Ratio ((%))
--
-- Then edit your @layoutHook@ by adding the IM layout:
--
-- > myLayouts = IM (1%7) (ClassName "Tkabber") ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
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

-- $todo
-- All these items are questionable. Please let me know if you find them useful.
--
-- * shrink\/expand
--
-- * allow roster placement on the right side or even on top\/bottom
--
-- * use arbitrary layout instead of grid

data IM a = IM Rational Property deriving (Read, Show)

instance LayoutClass IM Window where
    description _ = "IM"
    doLayout (IM r prop) rect stack = do
        let ws = S.integrate stack
        let (masterRect, slaveRect) = splitHorizontallyBy r rect
        master <- findM (hasProperty prop) ws
        let positions = case master of
                Just w -> (w, masterRect) : arrange slaveRect (filter (w /=) ws)
                Nothing -> arrange rect ws
        return (positions, Nothing)

-- | Like find, but works with monadic computation instead of pure function.
findM :: Monad m => (a-> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do { b <- f x; if b then return (Just x) else findM f xs }
