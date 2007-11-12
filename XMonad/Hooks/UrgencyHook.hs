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
                                 NoUrgencyHook(..), StdoutUrgencyHook(..),
                                 dzenUrgencyHook, DzenUrgencyHook(..),
                                 seconds
                                 ) where

import XMonad
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutModifier
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.NamedWindows (getName)

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.Bits (testBit)
import Data.IORef
import Data.List (delete)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign (unsafePerformIO)

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
-- Wire your urgency hook into the layoutHook by use of the withUrgencyHook
-- function. For example, add this to your config record:
--
-- > , layoutHook = Layout $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
-- >                       $ layout
--
-- It shouldn't hurt to have the "withUrgencyHook $" at the outermost layer,
-- as above, as UrgencyHook is a LayoutModifier, and hence passes on any
-- messages sent to it.
--
-- If you want to modify your logHook to print out information about urgent windows,
-- the functions readUrgents and withUrgents are there to help you with that.
-- No example for you.

-- | Focuses the most recently urgent window. Good for what ails ya -- I mean, your keybindings.
-- Example keybinding:
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

instance UrgencyHook h Window => LayoutModifier (WithUrgencyHook h) Window where
    handleMess (WithUrgencyHook theHook) mess
      | Just PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w } <- fromMessage mess = do
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              if (testBit flags urgencyHintBit)
                then do
                  -- Note: Broken clients, such as Xchat2, will set the urgency flag multiple
                  -- times (perhaps in an effort to get the task bar to "flash"). If this
                  -- bothers you, please submit a bug report.
                  userCode $ urgencyHook theHook w
                  adjustUrgents (\ws -> if elem w ws then ws else w : ws)
                else do
                  -- Remove window from urgents list when client removes urgency status.
                  -- The client should do this, e.g., when it receives focus.
                  userCode $ nonUrgencyHook theHook w
                  adjustUrgents (delete w)
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

withUrgencyHook :: (UrgencyHook h Window, LayoutClass l Window) =>
                   h -> l Window -> ModifiedLayout (WithUrgencyHook h) l Window
withUrgencyHook theHook = ModifiedLayout $ WithUrgencyHook theHook

class (Read h, Show h) => UrgencyHook h a where
    urgencyHook, nonUrgencyHook :: h -> a -> X ()

    nonUrgencyHook _ _ = return ()

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
    nonUrgencyHook _ w = io $ putStrLn $ "Not Urgent: " ++ show w
