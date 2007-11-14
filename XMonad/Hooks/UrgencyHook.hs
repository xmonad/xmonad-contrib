{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.UrgencyHook
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

module XMonad.Hooks.UrgencyHook (
                                 -- * Usage
                                 -- $usage
                                 withUrgencyHook,
                                 focusUrgent,
                                 readUrgents, withUrgents,
                                 urgencyLayoutHook,
                                 NoUrgencyHook(..), StdoutUrgencyHook(..),
                                 dzenUrgencyHook, DzenUrgencyHook(..),
                                 seconds
                                 ) where

import XMonad
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutModifier hiding (hook)
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.NamedWindows (getName)

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.Bits (testBit, clearBit)
import Data.IORef
import Data.List ((\\), delete)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Foreign (unsafePerformIO)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage
-- To wire this up, first add:
--
-- > import XMonad.Hooks.UrgencyHook
--
-- to your import list in your config file. Now, choose an urgency hook. If
-- you're just interested in displaying the urgency state in your custom
-- logHook, then choose NoUrgencyHook. Otherwise, you may use the provided
-- dzenUrgencyHook, or write your own.
--
-- Enable your urgency hook by wrapping your config record in a call to
-- withUrgencyHook. For example:
--
-- > main = xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
-- >               $ defaultConfig
--
-- If you want to modify your logHook to print out information about urgent windows,
-- the functions readUrgents and withUrgents are there to help you with that.
-- No example for you.

-- | This is the preferred method of enabling an urgency hook. It will prepend
-- an action to your logHook that remove visible windows from the list of urgent
-- windows. If you don't like that behavior, you may use urgencyLayoutHook instead.
withUrgencyHook :: (LayoutClass l Window, UrgencyHook h Window) =>
                   h -> XConfig l -> XConfig (ModifiedLayout (WithUrgencyHook h) l)
withUrgencyHook hook conf = conf { layoutHook = urgencyLayoutHook hook $ layoutHook conf
                                 , logHook = removeVisiblesFromUrgents >> logHook conf
                                 }

-- | The logHook action used by withUrgencyHook.
removeVisiblesFromUrgents :: X ()
removeVisiblesFromUrgents = do
    visibles <- gets mapped
    adjustUrgents (\\ (S.toList visibles))

-- | Focuses the most recently urgent window. Good for what ails ya -- I mean, your keybindings.
-- Example keybinding:
--
-- > , ((modMask              , xK_BackSpace), focusUrgent)
focusUrgent :: X ()
focusUrgent = withUrgents $ flip whenJust (windows . W.focusWindow) . listToMaybe

-- | Stores the global set of all urgent windows, across workspaces. Not exported -- use
-- @readUrgents@ or @withUrgents@ instead.
{-# NOINLINE urgents #-}
urgents :: IORef [Window]
urgents = unsafePerformIO (newIORef [])
-- (Hey, I don't like it any more than you do.)

readUrgents :: X [Window]
readUrgents = io $ readIORef urgents

withUrgents :: ([Window] -> X a) -> X a
withUrgents f = readUrgents >>= f

data WithUrgencyHook h a = WithUrgencyHook h deriving (Read, Show)

-- The Non-ICCCM Manifesto:
-- Note: Some non-standard choices have been made in this implementation to
-- account for the fact that things are different in a tiling window manager:
--   1. Several clients (e.g. Xchat2, rxvt-unicode) set the urgency flag
--      9 or 10 times in a row. This would, in turn, trigger urgencyHook repeatedly.
--      so in order to prevent that, we immediately clear the urgency flag.
--   2. In normal window managers, windows may overlap, so clients wait for focus to
--      be set before urgency is cleared. In a tiling WM, it's sufficient to be able
--      see the window, since we know that means you can see it completely.
--   3. The urgentOnBell setting in rxvt-unicode sets urgency even when the window
--      has focus, and won't clear until it loses and regains focus. This is stupid.
-- In order to account for these quirks, we clear the urgency bit immediately upon
-- receiving notification (thus suppressing the repeated notifications) and track
-- the list of urgent windows ourselves, allowing us to clear urgency when a window
-- is visible, and not to set urgency if a window is visible.
-- If you have a better idea, please, let us know!
instance UrgencyHook h Window => LayoutModifier (WithUrgencyHook h) Window where
    handleMess (WithUrgencyHook hook) mess
      | Just PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w } <- fromMessage mess = do
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              wmh@WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              when (testBit flags urgencyHintBit) $ do
                  -- Call the urgencyHook.
                  userCode $ urgencyHook hook w
                  -- Clear the bit to prevent repeated notifications, as described above.
                  io $ setWMHints dpy w wmh { wmh_flags = clearBit flags urgencyHintBit }
                  -- Add to list of urgents.
                  adjustUrgents (\ws -> if elem w ws then ws else w : ws)
                  -- Call logHook after IORef has been modified.
                  userCode =<< asks (logHook . config)
          return Nothing
      | Just DestroyWindowEvent {ev_window = w} <- fromMessage mess = do
          adjustUrgents (delete w)
          return Nothing
      | otherwise =
          return Nothing

adjustUrgents :: ([Window] -> [Window]) -> X ()
adjustUrgents f = io $ modifyIORef urgents f

urgencyLayoutHook :: (UrgencyHook h Window, LayoutClass l Window) =>
                   h -> l Window -> ModifiedLayout (WithUrgencyHook h) l Window
urgencyLayoutHook hook = ModifiedLayout $ WithUrgencyHook hook

--------------------------------------------------------------------------------
-- Urgency Hooks

-- | The class definition, and some pre-defined instances.

class (Read h, Show h) => UrgencyHook h a where
    urgencyHook :: h -> a -> X ()

data NoUrgencyHook = NoUrgencyHook deriving (Read, Show)

instance UrgencyHook NoUrgencyHook Window where
    urgencyHook _ _ = return ()

data DzenUrgencyHook = DzenUrgencyHook { duration :: Int, args :: [String] }
    deriving (Read, Show)

instance UrgencyHook DzenUrgencyHook Window where
    urgencyHook DzenUrgencyHook { duration = d, args = a } w = do
        visibles <- gets mapped
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name visibles)
      where flash name visibles index =
                  when (not $ S.member w visibles) $
                  dzenWithArgs (show name ++ " requests your attention on workspace " ++ index) a d

-- | Flashes when a window requests your attention and you can't see it. Configurable
-- duration and args to dzen.
dzenUrgencyHook :: DzenUrgencyHook
dzenUrgencyHook = DzenUrgencyHook { duration = (5 `seconds`), args = [] }

-- For debugging purposes, really.
data StdoutUrgencyHook = StdoutUrgencyHook deriving (Read, Show)

instance UrgencyHook StdoutUrgencyHook Window where
    urgencyHook    _ w = io $ putStrLn $ "Urgent: " ++ show w
