{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.NoBorders
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

module XMonadContrib.NoBorders (
                                -- * Usage
                                -- $usage
                                noBorders,
                                smartBorders,
                                withBorder
                               ) where

import Control.Monad.State ( gets )
import Graphics.X11.Xlib

import XMonad
import XMonadContrib.LayoutModifier
import {-# SOURCE #-} Config (borderWidth)
import qualified StackSet as W

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.NoBorders
--
-- and modify the defaultLayouts to call noBorders on the layouts you want to lack
-- borders
--
-- > defaultLayouts = [ noBorders full, ... ]

-- %import XMonadContrib.NoBorders
-- %layout -- prepend noBorders to default layouts above to remove their borders, like so:
-- %layout , noBorders full

-- todo, use an InvisibleList.
data WithBorder a = WithBorder Dimension [a] deriving ( Read, Show )

instance LayoutModifier WithBorder Window where
    modifierDescription (WithBorder 0 _) = "NoBorders"
    modifierDescription (WithBorder n _) = "Borders " ++ show n

    unhook (WithBorder _ s) = setBorders borderWidth s

    redoLayout (WithBorder n s) _ stack wrs = do
        setBorders borderWidth s
        setBorders n ws
        return (wrs, Just $ WithBorder n ws)
     where
        ws = map fst wrs

noBorders :: LayoutClass l Window => l Window -> ModifiedLayout WithBorder l Window
noBorders = ModifiedLayout $ WithBorder 0 []

withBorder :: LayoutClass l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout $ WithBorder b []

setBorders :: Dimension -> [Window] -> X ()
setBorders bw ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

data SmartBorder a = SmartBorder [a] deriving (Read, Show)

instance LayoutModifier SmartBorder Window where
    modifierDescription _ = "SmartBorder"

    unhook (SmartBorder s) = setBorders borderWidth s

    redoLayout (SmartBorder s) _ stack wrs = do
        ss <- gets (W.screens . windowset)
        setBorders borderWidth s

        if singleton ws && singleton ss
            then do setBorders 0 ws; return (wrs, Just $ SmartBorder ws)
            else return (wrs, Just $ SmartBorder [])
     where
        ws = map fst wrs
        singleton = null . drop 1

smartBorders = ModifiedLayout (SmartBorder [])
