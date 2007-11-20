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

import Control.Monad.State (gets)
import Control.Monad.Reader (asks)
import Graphics.X11.Xlib

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import Data.List ((\\))

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

-- %import XMonad.Layout.NoBorders
-- %layout -- prepend noBorders to default layouts above to remove their borders, like so:
-- %layout , noBorders Full

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

noBorders :: LayoutClass l Window => l Window -> ModifiedLayout WithBorder l Window
noBorders = ModifiedLayout $ WithBorder 0 []

withBorder :: LayoutClass l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout $ WithBorder b []

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

data SmartBorder a = SmartBorder [a] deriving (Read, Show)

instance LayoutModifier SmartBorder Window where
    unhook (SmartBorder s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (SmartBorder s) _ _ wrs = do
        ss <- gets (W.screens . windowset)

        if singleton ws && singleton ss
            then do
                asks (borderWidth . config) >>= setBorders (s \\ ws)
                setBorders ws 0
                return (wrs, Just $ SmartBorder ws)
            else do
                asks (borderWidth . config) >>= setBorders s
                return (wrs, Just $ SmartBorder [])
     where
        ws = map fst wrs
        singleton = null . drop 1

--
-- | You can cleverly set no borders on a range of layouts, using a
-- layoutHook like so:
--
-- > layoutHook = smartBorders $ tiled ||| Mirror tiled ||| ...
--
smartBorders :: LayoutClass l a => l a -> ModifiedLayout SmartBorder l a
smartBorders = ModifiedLayout (SmartBorder [])
