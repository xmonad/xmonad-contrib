{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}

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
                                 withUrgencyHook,
                                 readUrgents,
                                 withUrgents
                                 ) where

import {-# SOURCE #-} Config (urgencyHook, logHook)
import XMonad
import XMonadContrib.LayoutModifier

import Control.Monad (when)
import Control.Monad.State (gets)
import Data.Bits (testBit, clearBit)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as S
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign (unsafePerformIO)

-- $usage
-- To wire this up, add:
--
-- > import XMonadContrib.UrgencyHook
--
-- to your import list in Config. Change your defaultLayout such that
-- withUrgencyHook is applied along the chain. Mine, for example:
--
-- > layoutHook = Layout $ withUrgencyHook $ windowNavigation $
-- >                       Select layouts
--
-- It shouldn't hurt to have the "withUrgencyHook $" at the outermost layer,
-- as above, as UrgencyHook is a LayoutModifier, and hence passes on any
-- messages sent to it. Next, add your actual urgencyHook to Config. This
-- needs to take a Window and return an X () action. Here's an example:
--
-- > import XMonadContrib.Dzen
-- ...
-- > urgencyHook :: Window -> X ()
-- > urgencyHook = dzenUrgencyHook (5 `seconds`)
--
-- If you're comfortable with programming in the X monad, then you can build
-- whatever urgencyHook you like.  Finally, in order to make this compile,
-- open up your Config.hs-boot file and add the following to it:
--
-- > urgencyHook :: Window -> X ()
--
-- Compile!
--
-- You can also modify your logHook to print out information about urgent windows.
-- The functions readUrgents and withUrgents are there to help you with that.
-- No example for you.

-- | Stores the global set of all urgent windows, across workspaces. Not exported -- use
-- @readUrgents@ or @withUrgents@ instead.
{-# NOINLINE urgents #-}
urgents :: IORef (Set Window)
urgents = unsafePerformIO (newIORef S.empty)

readUrgents :: X (Set Window)
readUrgents = io $ readIORef urgents

withUrgents :: (Set Window -> X a) -> X a
withUrgents f = readUrgents >>= f

data WithUrgencyHook a = WithUrgencyHook deriving (Read, Show)

instance LayoutModifier WithUrgencyHook Window where
    handleMess _ mess
      | Just (PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w }) <- fromMessage mess = do
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              wmh@WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              when (testBit flags urgencyHintBit) $ do
                  urgencyHook w
                  -- Clear the urgency bit in the WMHints flags field. According to the
                  -- Xlib manual, the *client* is supposed to clear this flag when the urgency
                  -- has been resolved, but, Xchat2, for example, sets the WMHints several
                  -- times (e.g. causing the dzen to blink) unless it's cleared. XMonad is
                  -- not a typical WM, so we're just breaking one more rule, here.
                  io $ setWMHints dpy w wmh { wmh_flags = clearBit flags urgencyHintBit }
                  adjustUrgents (S.insert w)
                  logHook -- call logHook after IORef has been modified
              -- Doing the setWMHints triggers another propertyNotify with the bit
              -- cleared, so we ignore that message. This has the potentially wrong
              -- effect of ignoring *all* urgency-clearing messages, some of which might
              -- be legitimate. Let's wait for bug reports on that, though.
          return Nothing
      | otherwise =
          return Nothing

    -- Clear the urgency bit and remove from the urgent list when the window becomes visible.
    redoLayout _ _ _ windowRects = do
      visibles <- gets mapped
      adjustUrgents (S.\\ visibles)
      return (windowRects, Nothing)

adjustUrgents :: (Set Window -> Set Window) -> X ()
adjustUrgents f = io $ modifyIORef urgents f

withUrgencyHook :: LayoutClass l Window => l Window -> ModifiedLayout WithUrgencyHook l Window
withUrgencyHook = ModifiedLayout WithUrgencyHook
