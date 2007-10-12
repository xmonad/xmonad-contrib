-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.UrgencyHook
-- Copyright   :  Devin Mullins <me@twifkak.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- UrgencyHook lets you configure an action to occur when a window demands
-- your attention. (In traditional WMs, this takes the form of "flashing"
-- on your "taskbar." Blech.)
--
-----------------------------------------------------------------------------

module XMonadContrib.UrgencyHook (
                                 -- * Usage
                                 -- $usage
                                 withUrgencyHook
                                 ) where

import {-# SOURCE #-} Config (urgencyHook)
import XMonad
import XMonadContrib.LayoutModifier

import Control.Monad (when)
import Data.Bits (testBit, clearBit)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- To wire this up, add:
--
-- > import XMonadContrib.UrgencyHook
--
-- to your import list in Config. Change your defaultLayout such that
-- withUrgencyHook is applied along the chain. Mine, for example:
--
-- > defaultLayout = Layout $ withUrgencyHook $ windowNavigation wnConfig $
-- >                          LayoutSelection defaultLayouts
--
-- It shouldn't hurt to have the "withUrgencyHook $" at the outermost layer,
-- as above, as UrgencyHook is a LayoutModifier, and hence passes on any
-- messages sent to it. Next, add your actual urgencyHook to Config. This
-- needs to take a Window and return an X () action. Here's an example:
--
-- > import Dzen (dzen)
-- > import NamedWindows (getName)
-- ...
-- > urgencyHook :: Window -> X ()
-- > urgencyHook w = do
-- >     name <- getName w
-- >     ws <- gets windowset
-- >     whenJust (W.findIndex w ws) (flash name ws)
-- >   where flash name ws index =
-- >               when (index /= W.tag (W.workspace (W.current ws))) $
-- >               dzen (show name ++ " requests your attention on workspace " ++ show index)
--
-- This example stands on the shoulders of the NamedWindows and Dzen modules,
-- but you can build whatever urgencyHook you like. Finally, in order to make
-- this compile, open up your Config.hs-boot file and add the following to it:
--
-- > urgencyHook :: Window -> X ()
--
-- Compile!

data WithUrgencyHook a = WithUrgencyHook deriving (Read, Show)

instance LayoutModifier WithUrgencyHook Window where
    handleMess _ mess =
      let event = fromMessage mess :: Maybe Event in do
      case event of
          Just (PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w }) ->
              when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
                  wmh@WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
                  when (testBit flags urgencyHintBit) $ do
                      urgencyHook w
                      -- Is clearing the bit really necessary? Xlib manual advises it.
                      io $ setWMHints dpy w wmh { wmh_flags = clearBit flags urgencyHintBit }
                      return ()
          _ -> return ()
      return Nothing

withUrgencyHook :: LayoutClass l Window => l Window -> ModifiedLayout WithUrgencyHook l Window
withUrgencyHook = ModifiedLayout WithUrgencyHook
