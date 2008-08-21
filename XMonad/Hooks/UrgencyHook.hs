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

                                 -- ** Pop up a temporary dzen
                                 -- $temporary

                                 -- ** Highlight in existing dzen
                                 -- $existing

                                 -- ** Useful keybinding
                                 -- $keybinding

                                 -- ** Note
                                 -- $note

                                 -- * Troubleshooting
                                 -- $troubleshooting

                                 -- * Example: Setting up irssi + rxvt-unicode
                                 -- $example

                                 -- ** Configuring irssi
                                 -- $irssi

                                 -- ** Configuring screen
                                 -- $screen

                                 -- ** Configuring rxvt-unicode
                                 -- $urxvt

                                 -- ** Configuring xmonad
                                 -- $xmonad

                                 -- * Stuff for your config file:
                                 withUrgencyHook, withUrgencyHookC,
                                 UrgencyConfig(..), urgencyConfig,
                                 SuppressWhen(..),
                                 focusUrgent,
                                 dzenUrgencyHook,
                                 DzenUrgencyHook(..), seconds,
                                 NoUrgencyHook(..),
                                 FocusHook(..),
                                 -- * Stuff for developers:
                                 readUrgents, withUrgents,
                                 StdoutUrgencyHook(..),
                                 SpawnUrgencyHook(..),
                                 UrgencyHook(urgencyHook)
                                 ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.EventHook
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.NamedWindows (getName)

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (testBit)
import Data.IORef
import Data.List ((\\), delete)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S
import Foreign (unsafePerformIO)

-- $usage
--
-- To wire this up, first add:
--
-- > import XMonad.Hooks.UrgencyHook
--
-- to your import list in your config file. Now, you have a decision to make:
-- When a window deems itself urgent, do you want to pop up a temporary dzen
-- bar telling you so, or do you have an existing dzen wherein you would like to
-- highlight urgent workspaces?

-- $temporary
--
-- Enable your urgency hook by wrapping your config record in a call to
-- 'withUrgencyHook'. For example:
--
-- > main = xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
-- >               $ defaultConfig
--
-- This will pop up a dzen bar for five seconds telling you you've got an
-- urgent window.

-- $existing
--
-- In order for xmonad to track urgent windows, you must install an urgency hook.
-- You can use the above 'dzenUrgencyHook', or if you're not interested in the
-- extra popup, install NoUrgencyHook, as so:
--
-- > main = xmonad $ withUrgencyHook NoUrgencyHook
-- >               $ defaultConfig
--
-- Now, your "XMonad.Hooks.DynamicLog" must be set up to display the urgent
-- windows. If you're using the 'dzen' or 'dzenPP' functions from that module,
-- then you should be good. Otherwise, you want to figure out how to set
-- 'ppUrgent'.

-- $keybinding
--
-- You can set up a keybinding to jump to the window that was recently marked
-- urgent. See an example at 'focusUrgent'.

-- $note
-- Note: UrgencyHook installs itself as a LayoutModifier, so if you modify your
-- urgency hook and restart xmonad, you may need to rejigger your layout by
-- hitting mod-shift-space.

-- $troubleshooting
--
-- There are three steps to get right:
--
-- 1. The X client must set the UrgencyHint flag. How to configure this
--    depends on the application. If you're using a terminal app, this is in
--    two parts:
--
--      * The console app must send a ^G (bell). In bash, a helpful trick is
--        @sleep 1; echo -e \'\a\'@.
--
--      * The terminal must convert the bell into UrgencyHint.
--
-- 2. XMonad must be configured to notice UrgencyHints. If you've added
--    withUrgencyHook, you may need to hit mod-shift-space to reset the layout.
--
-- 3. The dzen must run when told. Run @dzen2 -help@ and make sure that it
--    supports all of the arguments you told DzenUrgencyHook to pass it. Also,
--    set up a keybinding to the 'dzen' action in "XMonad.Util.Dzen" to test
--    if that works.
--
-- As best you can, try to isolate which one(s) of those is failing.

-- $example
--
-- This is a commonly asked example. By default, the window doesn't get flagged
-- urgent when somebody messages you in irssi. You will have to configure some
-- things. If you're using different tools than this, your mileage will almost
-- certainly vary. (For example, in Xchat2, it's just a simple checkbox.)

-- $irssi
-- @Irssi@ is not an X11 app, so it can't set the @UrgencyHint@ flag on @XWMHints@.
-- However, on all console applications is bestown the greatest of all notification
-- systems: the bell. That's right, Ctrl+G, ASCII code 7, @echo -e '\a'@, your
-- friend, the bell. To configure @irssi@ to send a bell when you receive a message:
--
-- > /set beep_msg_level MSGS NOTICES INVITES DCC DCCMSGS HILIGHT
--
-- Consult your local @irssi@ documentation for more detail.

-- $screen
-- A common way to run @irssi@ is within the lovable giant, @screen@. Some distros
-- (e.g. Ubuntu) like to configure @screen@ to trample on your poor console
-- applications -- in particular, to turn bell characters into evil, smelly
-- \"visual bells.\" To turn this off, add:
--
-- > vbell off # or remove the existing 'vbell on' line
--
-- to your .screenrc, or hit @C-a C-g@ within a running @screen@ session for an
-- immediate but temporary fix.

-- $urxvt
-- Rubber, meet road. Urxvt is the gateway between console apps and X11. To tell
-- urxvt to set an @UrgencyHint@ when it receives a bell character, first, have
-- an urxvt version 8.3 or newer, and second, set the following in your
-- @.Xdefaults@:
--
-- > urxvt.urgentOnBell: true
--
-- Depending on your setup, you may need to @xrdb@ that.

-- $xmonad
-- Hopefully you already read the section on how to configure xmonad. If not,
-- hopefully you know where to find it.

-- | This is the method to enable an urgency hook. It suppresses urgency status
-- for windows that are currently visible. If you'd like to change that behavior,
-- use 'withUrgencyHookC'.
withUrgencyHook :: (LayoutClass l Window, UrgencyHook h) =>
                   h -> XConfig l -> XConfig (HandleEvent (WithUrgencyHook h) l)
withUrgencyHook hook conf = withUrgencyHookC hook urgencyConfig conf

-- | If you'd like to configure *when* to trigger the urgency hook, call this
-- function with a custom 'UrgencyConfig'. Or, by example:
--
-- > withUrgencyHookC dzenUrgencyHook { ... } urgencyConfig { suppressWhen = Focused }
--
-- (Don't type the @...@, you dolt.) See documentation on your options at 'SuppressWhen'.
withUrgencyHookC :: (LayoutClass l Window, UrgencyHook h) =>
                    h -> UrgencyConfig -> XConfig l -> XConfig (HandleEvent (WithUrgencyHook h) l)
withUrgencyHookC hook urgConf conf = conf {
        layoutHook = eventHook (WithUrgencyHook hook urgConf) $ layoutHook conf,
        logHook = cleanupUrgents (suppressWhen urgConf) >> logHook conf
    }

-- | Global configuration, applicable to all types of 'UrgencyHook'.
data UrgencyConfig = UrgencyConfig
    { suppressWhen :: SuppressWhen -- ^ see 'SuppressWhen' for options
    } deriving (Read, Show)

-- | The default 'UrgencyConfig'. Use a variation of this in your config just
-- as you use a variation of defaultConfig for your xmonad definition.
urgencyConfig :: UrgencyConfig
urgencyConfig = UrgencyConfig { suppressWhen = Visible }

-- | A set of choices as to /when/ you should (or rather, shouldn't) be notified of an urgent window.
-- The default is 'Visible'. Prefix each of the following with \"don't bug me when\":
data SuppressWhen = Visible  -- ^ the window is currently visible
                  | OnScreen -- ^ the window is on the currently focused physical screen
                  | Focused  -- ^ the window is currently focused
                  | Never    -- ^ ... aww, heck, go ahead and bug me, just in case.
                  deriving (Read, Show)

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

data WithUrgencyHook h = WithUrgencyHook h UrgencyConfig deriving (Read, Show)

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
    handleEvent wuh event =
      case event of
        PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w } -> do
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              if (testBit flags urgencyHintBit) then do
                  -- Add to list of urgents.
                  adjustUrgents (\ws -> if elem w ws then ws else w : ws)
                  -- Call the urgencyHook.
                  callUrgencyHook wuh w
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

callUrgencyHook :: UrgencyHook h => WithUrgencyHook h -> Window -> X ()
callUrgencyHook (WithUrgencyHook hook UrgencyConfig { suppressWhen = sw }) w =
    whenX (not <$> shouldSuppress sw w)
          (userCode $ urgencyHook hook w)

shouldSuppress :: SuppressWhen -> Window -> X Bool
shouldSuppress sw w = elem w <$> suppressibleWindows sw

cleanupUrgents :: SuppressWhen -> X ()
cleanupUrgents sw = do
    suppressibles <- suppressibleWindows sw
    adjustUrgents (\\ suppressibles)

suppressibleWindows :: SuppressWhen -> X [Window]
suppressibleWindows Visible  = gets $ S.toList . mapped
suppressibleWindows OnScreen = gets $ W.index . windowset
suppressibleWindows Focused  = gets $ maybeToList . W.peek . windowset
suppressibleWindows Never    = return []

--------------------------------------------------------------------------------
-- Urgency Hooks

-- | The class definition, and some pre-defined instances.

class (Read h, Show h) => UrgencyHook h where
    urgencyHook :: h -> Window -> X ()

data NoUrgencyHook = NoUrgencyHook deriving (Read, Show)

instance UrgencyHook NoUrgencyHook where
    urgencyHook _ _ = return ()

-- | Your set of options for configuring a dzenUrgencyHook.
data DzenUrgencyHook = DzenUrgencyHook {
                         duration :: Int, -- ^ number of microseconds to display the dzen
                                          --   (hence, you'll probably want to use 'seconds')
                         args :: [String] -- ^ list of extra args (as 'String's) to pass to dzen
                       }
    deriving (Read, Show)

instance UrgencyHook DzenUrgencyHook where
    urgencyHook DzenUrgencyHook { duration = d, args = a } w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  dzenWithArgs (show name ++ " requests your attention on workspace " ++ index) a d

{- | A hook which will automatically send you to anything which sets the urgent
  flag (as opposed to printing some sort of message. You would use this as
  usual, eg.

  > withUrgencyHook FocusHook $ myconfig { ...
-}
data FocusHook = FocusHook deriving (Read, Show)

instance UrgencyHook FocusHook where
    urgencyHook _ _ = focusUrgent

-- | Flashes when a window requests your attention and you can't see it.
-- Defaults to a duration of five seconds, and no extra args to dzen.
-- See 'DzenUrgencyHook'.
dzenUrgencyHook :: DzenUrgencyHook
dzenUrgencyHook = DzenUrgencyHook { duration = (5 `seconds`), args = [] }

-- | Spawn a commandline thing, appending the window id to the prefix string
-- you provide. (Make sure to add a space if you need it.) Do your crazy
-- xcompmgr thing.
newtype SpawnUrgencyHook = SpawnUrgencyHook String deriving (Read, Show)

instance UrgencyHook SpawnUrgencyHook where
    urgencyHook (SpawnUrgencyHook prefix) w = spawn $ prefix ++ show w

-- | For debugging purposes, really.
data StdoutUrgencyHook = StdoutUrgencyHook deriving (Read, Show)

instance UrgencyHook StdoutUrgencyHook where
    urgencyHook    _ w = io $ putStrLn $ "Urgent: " ++ show w
