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
-- your attention. (In traditional WMs, this takes the form of \"flashing\"
-- on your \"taskbar.\" Blech.)
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
                                 SpawnUrgencyHook(..),
                                 dzenUrgencyHook, DzenUrgencyHook(..),
                                 UrgencyHook(urgencyHook),
                                 whenShouldTrigger, seconds,
                                 SuppressWhen(..)
                                 ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.EventHook
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.NamedWindows (getName)

import Control.Monad (when)
import Data.Bits (testBit)
import Data.IORef
import Data.List ((\\), delete)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Foreign (unsafePerformIO)

-- $usage
-- To wire this up, first add:
--
-- > import XMonad.Hooks.UrgencyHook
--
-- to your import list in your config file. Now, choose an urgency hook. If
-- you're just interested in displaying the urgency state in your custom
-- logHook, then choose NoUrgencyHook. Otherwise, you may use the provided
-- 'dzenUrgencyHook', or write your own.
--
-- Enable your urgency hook by wrapping your config record in a call to
-- 'withUrgencyHook'. For example:
--
-- > main = xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
-- >               $ defaultConfig
--
-- If you want to modify your logHook to print out information about urgent windows,
-- the functions 'readUrgents' and 'withUrgents' are there to help you with that.
-- No example for you.

-- | This is the preferred method of enabling an urgency hook. It will prepend
-- an action to your logHook that removes visible windows from the list of urgent
-- windows. If you don't like that behavior, you may use 'urgencyLayoutHook' instead.
withUrgencyHook :: (LayoutClass l Window, UrgencyHook h) =>
                   h -> XConfig l -> XConfig (HandleEvent (WithUrgencyHook h) l)
withUrgencyHook hook conf = conf { layoutHook = urgencyLayoutHook hook $ layoutHook conf
                                 , logHook = removeVisiblesFromUrgents >> logHook conf
                                 }

-- | The logHook action used by 'withUrgencyHook'.
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
-- 'readUrgents' or 'withUrgents' instead.
{-# NOINLINE urgents #-}
urgents :: IORef [Window]
urgents = unsafePerformIO (newIORef [])
-- (Hey, I don't like it any more than you do.)

-- | X action that returns a list of currently urgent windows. You might use
-- it, or 'withUrgents', in your custom logHook, to display the workspaces that
-- contain urgent windows.
readUrgents :: X [Window]
readUrgents = io $ readIORef urgents

-- | An HOF version of 'readUrgents', for those who prefer that sort of thing.
withUrgents :: ([Window] -> X a) -> X a
withUrgents f = readUrgents >>= f

data WithUrgencyHook h = WithUrgencyHook h deriving (Read, Show)

-- The Non-ICCCM Manifesto:
-- Note: Some non-standard choices have been made in this implementation to
-- account for the fact that things are different in a tiling window manager:
--   1. In normal window managers, windows may overlap, so clients wait for focus to
--      be set before urgency is cleared. In a tiling WM, it's sufficient to be able
--      see the window, since we know that means you can see it completely.
--   2. The urgentOnBell setting in rxvt-unicode sets urgency even when the window
--      has focus, and won't clear until it loses and regains focus. This is stupid.
-- In order to account for these quirks, we track the list of urgent windows
-- ourselves, allowing us to clear urgency when a window is visible, and not to
-- set urgency if a window is visible. If you have a better idea, please, let us
-- know!
instance UrgencyHook h => EventHook (WithUrgencyHook h) where
    handleEvent (WithUrgencyHook hook) event =
      case event of
        PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w } -> do
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              if (testBit flags urgencyHintBit) then do
                  -- Call the urgencyHook.
                  userCode $ urgencyHook hook w
                  -- Add to list of urgents.
                  adjustUrgents (\ws -> if elem w ws then ws else w : ws)
                  -- Call logHook after IORef has been modified.
                  userCode =<< asks (logHook . config)
                else do
                  -- Remove from list of urgents.
                  adjustUrgents (delete w)
                  -- Call logHook after IORef has been modified.
                  userCode =<< asks (logHook . config)
        DestroyWindowEvent {ev_window = w} -> do
          adjustUrgents (delete w)
        _ ->
          return ()

adjustUrgents :: ([Window] -> [Window]) -> X ()
adjustUrgents f = io $ modifyIORef urgents f

urgencyLayoutHook :: (UrgencyHook h, LayoutClass l Window) =>
                   h -> l Window -> HandleEvent (WithUrgencyHook h) l Window
urgencyLayoutHook hook = eventHook $ WithUrgencyHook hook

--------------------------------------------------------------------------------
-- Urgency Hooks

-- | The class definition, and some pre-defined instances.

-- TODO: factor SuppressWhen stuff into WithUrgencyHook

data SuppressWhen = Visible | OnScreen | Focused | Never deriving (Read, Show)

shouldSuppress :: SuppressWhen -> Window -> X Bool
shouldSuppress Visible  w = gets $ S.member w . mapped
shouldSuppress OnScreen w = gets $ elem w . W.index . windowset
shouldSuppress Focused  w = gets $ maybe False (w ==) . W.peek . windowset
shouldSuppress Never    _ = return False

-- | Convenience method for those writing UrgencyHooks.
whenShouldTrigger :: SuppressWhen -> Window -> X () -> X ()
whenShouldTrigger sw w = whenX (not `fmap` shouldSuppress sw w)

class (Read h, Show h) => UrgencyHook h where
    urgencyHook :: h -> Window -> X ()

data NoUrgencyHook = NoUrgencyHook deriving (Read, Show)

instance UrgencyHook NoUrgencyHook where
    urgencyHook _ _ = return ()

data DzenUrgencyHook = DzenUrgencyHook { duration :: Int,
                                         args :: [String],
                                         suppressWhen :: SuppressWhen }
                       deriving (Read, Show)

instance UrgencyHook DzenUrgencyHook where
    urgencyHook DzenUrgencyHook { duration = d, args = a, suppressWhen = sw } w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  whenShouldTrigger sw w $
                  dzenWithArgs (show name ++ " requests your attention on workspace " ++ index) a d

-- | Flashes when a window requests your attention and you can't see it. Configurable
-- duration and args to dzen, and when to suppress the urgency flash.
dzenUrgencyHook :: DzenUrgencyHook
dzenUrgencyHook = DzenUrgencyHook { duration = (5 `seconds`), args = [], suppressWhen = Visible }

-- | Spawn a commandline thing, appending the window id to the prefix string
-- you provide. (Make sure to add a space if you need it.) Do your crazy
-- xcompmgr thing.
newtype SpawnUrgencyHook = SpawnUrgencyHook String deriving (Read, Show)

instance UrgencyHook SpawnUrgencyHook where
    urgencyHook (SpawnUrgencyHook prefix) w = spawn $ prefix ++ show w

-- For debugging purposes, really.
data StdoutUrgencyHook = StdoutUrgencyHook deriving (Read, Show)

instance UrgencyHook StdoutUrgencyHook where
    urgencyHook    _ w = io $ putStrLn $ "Urgent: " ++ show w
