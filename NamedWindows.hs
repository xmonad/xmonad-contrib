module XMonadContrib.NamedWindows ( NamedWindow, getName, withNamedWindow, unName ) where

-- This module allows you to associate the X titles of windows with
-- them.  See XMonadContrib.Mosaic for an example of its use.

import Control.Monad.Reader ( asks )
import Control.Monad.State ( gets )

import qualified StackSet as W ( peek )

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( fetchName )

import XMonad

data NamedWindow = NW !String !Window
instance Eq NamedWindow where
    (NW s _) == (NW s' _) = s == s'
instance Ord NamedWindow where
    compare (NW s _) (NW s' _) = compare s s'
instance Show NamedWindow where
    show (NW n _) = n

getName :: Window -> X NamedWindow
getName w = asks display >>= \d -> do n <- maybe "" id `fmap` io (fetchName d w)
                                      return $ NW n w

unName :: NamedWindow -> Window
unName (NW _ w) = w

withNamedWindow :: (NamedWindow -> X ()) -> X ()
withNamedWindow f = do ws <- gets windowset
                       whenJust (W.peek ws) $ \w -> getName w >>= f
