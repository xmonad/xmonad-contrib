{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.NoBorders
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Make a given layout display without borders.  This is useful for
-- full-screen or tabbed layouts, where you don't really want to waste a
-- couple of pixels of real estate just to inform yourself that the visible
-- window has focus.
--
-----------------------------------------------------------------------------

module XMonad.Layout.NoBorders (
                                -- * Usage
                                -- $usage
                                noBorders,
                                smartBorders,
                                withBorder
                               ) where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import Data.List ((\\))
import qualified Data.Map as M

-- $usage
-- You can use this module with the following in your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.NoBorders
--
-- and modify the layouts to call noBorders on the layouts you want to lack
-- borders:
--
-- > layoutHook = ... ||| noBorders Full ||| ...
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- todo, use an InvisibleList.
data WithBorder a = WithBorder Dimension [a] deriving ( Read, Show )

instance LayoutModifier WithBorder Window where
    unhook (WithBorder _ s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (WithBorder n s) _ _ wrs = do
        asks (borderWidth . config) >>= setBorders (s \\ ws)
        setBorders ws n
        return (wrs, Just $ WithBorder n ws)
     where
        ws = map fst wrs

-- | Removes all window borders from the specified layout.
noBorders :: LayoutClass l Window => l Window -> ModifiedLayout WithBorder l Window
noBorders = withBorder 0

-- | Forces a layout to use the specified border width. 'noBorders' is
-- equivalent to @'withBorder' 0@.
withBorder :: LayoutClass l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout $ WithBorder b []

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

data SmartBorder a = SmartBorder [a] deriving (Read, Show)

instance LayoutModifier SmartBorder Window where
    unhook (SmartBorder s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (SmartBorder s) _ st wrs = do
        wset <- gets windowset
        let managedwindows = W.integrate st
            screens = filter (nonzerorect . screenRect . W.screenDetail) . W.screens $ wset
            ws = tiled ++ floating
            tiled = case filter (`elem` managedwindows) $ map fst wrs of
                [w] | singleton screens -> [w]
                _ -> []
            floating =
                [ w |
                    (w, W.RationalRect px py wx wy) <- M.toList . W.floating $ wset,
                    px <= 0, py <= 0,
                    wx + px >= 1, wy + py >= 1
                ]
        asks (borderWidth . config) >>= setBorders (s \\ ws)
        setBorders ws 0
        return (wrs, Just $ SmartBorder ws)
     where
        singleton = null . drop 1
        nonzerorect (Rectangle _ _ 0 0) = False
        nonzerorect _ = True

-- | Removes the borders from a window under one of the following conditions:
--
--  * There is only one screen and only one window. In this case it's obvious
--  that it has the focus, so no border is needed.
--
--  * A floating window covers the entire screen (e.g. mplayer).
--
smartBorders :: LayoutClass l a => l a -> ModifiedLayout SmartBorder l a
smartBorders = ModifiedLayout (SmartBorder [])
