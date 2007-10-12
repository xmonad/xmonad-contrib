module XMonadContrib.UrgencyHook where

import {-# SOURCE #-} Config (urgencyHook)
import XMonad
import XMonadContrib.LayoutModifier

import Control.Monad (when)
import Data.Bits (testBit, clearBit)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- Oooh, spooky.
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
