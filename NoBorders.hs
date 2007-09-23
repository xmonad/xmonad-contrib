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
                                withBorder
                               ) where

import Control.Monad.State ( gets )
import Graphics.X11.Xlib

import XMonad
import XMonadContrib.LayoutHelpers
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

data WithBorder a = WithBorder Dimension deriving ( Read, Show )

instance LayoutModifier WithBorder a where
    hook (WithBorder b) = setborders b
    unhook (WithBorder _) = setborders borderWidth

noBorders :: Layout l a => l a -> ModifiedLayout WithBorder l a
noBorders = ModifiedLayout (WithBorder 0)

withBorder :: Layout l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout (WithBorder b)

setborders :: Dimension -> X ()
setborders bw = withDisplay $ \d ->
                do ws <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
                   mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws
