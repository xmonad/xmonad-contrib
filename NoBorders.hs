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
import Operations ( UnDoLayout(UnDoLayout) )
import qualified StackSet as W
import {-# SOURCE #-} Config (borderWidth)

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

noBorders :: Layout a -> Layout a
noBorders = withBorder 0

withBorder :: Dimension -> Layout a -> Layout a
withBorder bd l = l { doLayout = \r x -> setborders bd >> doLayout l r x
                    , modifyLayout = ml }
    where ml m | Just UnDoLayout == fromMessage m
                   = do setborders borderWidth
                        fmap (withBorder bd) `fmap` (modifyLayout l) m
               | otherwise = fmap (withBorder bd) `fmap` (modifyLayout l) m

setborders :: Dimension -> X ()
setborders bw = withDisplay $ \d ->
                do ws <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
                   mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws
