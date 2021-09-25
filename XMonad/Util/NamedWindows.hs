-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.NamedWindows
-- Description :  Associate the X titles of windows with them.
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module allows you to associate the X titles of windows with
-- them.
--
-----------------------------------------------------------------------------

module XMonad.Util.NamedWindows (
                                   -- * Usage
                                   -- $usage
                                   NamedWindow,
                                   getName,
                                   getNameWMClass,
                                   withNamedWindow,
                                   unName
                                  ) where

import Control.Exception as E
import XMonad.Prelude ( fromMaybe, listToMaybe, (>=>) )

import qualified XMonad.StackSet as W ( peek )


import XMonad

-- $usage
-- See "XMonad.Layout.Tabbed" for an example of its use.


data NamedWindow = NW !String !Window
instance Eq NamedWindow where
    (NW s _) == (NW s' _) = s == s'
instance Ord NamedWindow where
    compare (NW s _) (NW s' _) = compare s s'
instance Show NamedWindow where
    show (NW n _) = n

getName :: Window -> X NamedWindow
getName w = withDisplay $ \d -> do
    -- TODO, this code is ugly and convoluted -- clean it up
    let getIt = bracket getProp (xFree . tp_value) (fmap (`NW` w) . copy)

        getProp = (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                      `E.catch` \(SomeException _) -> getTextProperty d w wM_NAME

        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop

    io $ getIt `E.catch` \(SomeException _) ->  (`NW` w) . resName <$> getClassHint d w

-- | Get 'NamedWindow' using 'wM_CLASS'
getNameWMClass :: Window -> X NamedWindow
getNameWMClass w =
  withDisplay $ \d
    -- TODO, this code is ugly and convoluted -- clean it up
   -> do
    let getIt = bracket getProp (xFree . tp_value) (fmap (`NW` w) . copy)
        getProp = getTextProperty d w wM_CLASS
        copy prop =
          fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $
      getIt `E.catch` \(SomeException _) ->
        (`NW` w) . resName <$> getClassHint d w

unName :: NamedWindow -> Window
unName (NW _ w) = w

withNamedWindow :: (NamedWindow -> X ()) -> X ()
withNamedWindow f = do ws <- gets windowset
                       whenJust (W.peek ws) (getName >=> f)
