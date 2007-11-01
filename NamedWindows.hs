-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.NamedWindows
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module allows you to associate the X titles of windows with
-- them.
--
-----------------------------------------------------------------------------

module XMonadContrib.NamedWindows (
                                   -- * Usage
                                   -- $usage
                                   NamedWindow, 
                                   getName, 
                                   withNamedWindow, 
                                   unName 
                                  ) where

import Control.Monad.Reader ( asks )
import Control.Monad.State ( gets )

import qualified XMonad.StackSet as W ( peek )

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import XMonad

-- $usage
--   See "XMonadContrib.Mosaic" for an example of its use.


data NamedWindow = NW !String !Window
instance Eq NamedWindow where
    (NW s _) == (NW s' _) = s == s'
instance Ord NamedWindow where
    compare (NW s _) (NW s' _) = compare s s'
instance Show NamedWindow where
    show (NW n _) = n

getName :: Window -> X NamedWindow
getName w = asks display >>= \d -> do s <- io $ getClassHint d w
                                      n <- maybe (resName s) id `fmap` io (fetchName d w)
                                      return $ NW n w

unName :: NamedWindow -> Window
unName (NW _ w) = w

withNamedWindow :: (NamedWindow -> X ()) -> X ()
withNamedWindow f = do ws <- gets windowset
                       whenJust (W.peek ws) $ \w -> getName w >>= f
