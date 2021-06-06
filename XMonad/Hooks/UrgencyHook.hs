{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards, DeriveDataTypeable,
  FlexibleInstances #-}

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
                                 SuppressWhen(..), RemindWhen(..),
                                 focusUrgent, clearUrgents,
                                 dzenUrgencyHook,
                                 DzenUrgencyHook(..),
                                 NoUrgencyHook(..),
                                 BorderUrgencyHook(..),
                                 FocusHook(..),
                                 filterUrgencyHook, filterUrgencyHook',
                                 minutes, seconds,
                                 askUrgent, doAskUrgent,
                                 -- * Stuff for developers:
                                 readUrgents, withUrgents, clearUrgents',
                                 StdoutUrgencyHook(..),
                                 SpawnUrgencyHook(..),
                                 UrgencyHook(urgencyHook),
                                 Interval,
                                 borderUrgencyHook, focusHook, spawnUrgencyHook, stdoutUrgencyHook
                                 ) where

import XMonad
import XMonad.Prelude (fi, delete, fromMaybe, listToMaybe, maybeToList, when, (\\))
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers (windowTag)
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Timer (TimerId, startTimer, handleTimer)
import XMonad.Util.WindowProperties (getProp32)

import Data.Bits (testBit)
import qualified Data.Set as S
import System.IO (hPutStrLn, stderr)
import Foreign.C.Types (CLong)

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
-- >               $ def
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
-- >               $ def
--
-- Now, your "XMonad.Hooks.DynamicLog" must be set up to display the urgent
-- windows. If you're using the 'dzen' or 'dzenPP' functions from that module,
-- then you should be good. Otherwise, you want to figure out how to set
-- 'ppUrgent'.

-- $keybinding
--
-- You can set up a keybinding to jump to the window that was recently marked
-- urgent. See an example at 'focusUrgent'.

-- $troubleshooting
--
-- There are three steps to get right:
--
-- 1. The X client must set the UrgencyHint flag. How to configure this
--    depends on the application. If you're using a terminal app, this is in
--    two parts:
--
--      * The console app must send a ^G (bell). In bash, a helpful trick is
--        @sleep 1; echo -e \'\\a\'@.
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

-- | This is the method to enable an urgency hook. It uses the default
-- 'urgencyConfig' to control behavior. To change this, use 'withUrgencyHookC'
-- instead.
withUrgencyHook :: (LayoutClass l Window, UrgencyHook h) =>
                   h -> XConfig l -> XConfig l
withUrgencyHook hook conf = withUrgencyHookC hook urgencyConfig conf

-- | This lets you modify the defaults set in 'urgencyConfig'. An example:
--
-- > withUrgencyHookC dzenUrgencyHook { ... } urgencyConfig { suppressWhen = Focused }
--
-- (Don't type the @...@, you dolt.) See 'UrgencyConfig' for details on configuration.
withUrgencyHookC :: (LayoutClass l Window, UrgencyHook h) =>
                    h -> UrgencyConfig -> XConfig l -> XConfig l
withUrgencyHookC hook urgConf conf = conf {
        handleEventHook = \e -> handleEvent (WithUrgencyHook hook urgConf) e >> handleEventHook conf e,
        logHook = cleanupUrgents (suppressWhen urgConf) >> logHook conf,
        startupHook = cleanupStaleUrgents >> startupHook conf
    }

data Urgents = Urgents { fromUrgents :: [Window] } deriving (Read,Show,Typeable)

onUrgents :: ([Window] -> [Window]) -> Urgents -> Urgents
onUrgents f = Urgents . f . fromUrgents

instance ExtensionClass Urgents where
    initialValue = Urgents []
    extensionType = PersistentExtension

-- | Global configuration, applied to all types of 'UrgencyHook'. See
-- 'urgencyConfig' for the defaults.
data UrgencyConfig = UrgencyConfig
    { suppressWhen :: SuppressWhen -- ^ when to trigger the urgency hook
    , remindWhen   :: RemindWhen   -- ^ when to re-trigger the urgency hook
    } deriving (Read, Show)

-- | A set of choices as to /when/ you should (or rather, shouldn't) be notified of an urgent window.
-- The default is 'Visible'. Prefix each of the following with \"don't bug me when\":
data SuppressWhen = Visible  -- ^ the window is currently visible
                  | OnScreen -- ^ the window is on the currently focused physical screen
                  | Focused  -- ^ the window is currently focused
                  | Never    -- ^ ... aww, heck, go ahead and bug me, just in case.
                  deriving (Read, Show)

-- | A set of choices as to when you want to be re-notified of an urgent
-- window. Perhaps you focused on something and you miss the dzen popup bar. Or
-- you're AFK. Or you feel the need to be more distracted. I don't care.
--
-- The interval arguments are in seconds. See the 'minutes' helper.
data RemindWhen = Dont                    -- ^ triggering once is enough
                | Repeatedly Int Interval -- ^ repeat <arg1> times every <arg2> seconds
                | Every Interval          -- ^ repeat every <arg1> until the urgency hint is cleared
                deriving (Read, Show)

-- | A prettified way of multiplying by 60. Use like: @(5 `minutes`)@.
minutes :: Rational -> Rational
minutes secs = secs * 60

-- | The default 'UrgencyConfig'. suppressWhen = Visible, remindWhen = Dont.
-- Use a variation of this in your config just as you use a variation of
-- 'def' for your xmonad definition.
urgencyConfig :: UrgencyConfig
urgencyConfig = UrgencyConfig { suppressWhen = Visible, remindWhen = Dont }

-- | Focuses the most recently urgent window. Good for what ails ya -- I mean, your keybindings.
-- Example keybinding:
--
-- > , ((modm              , xK_BackSpace), focusUrgent)
focusUrgent :: X ()
focusUrgent = withUrgents $ flip whenJust (windows . W.focusWindow) . listToMaybe

-- | Just makes the urgents go away.
-- Example keybinding:
--
-- > , ((modm .|. shiftMask, xK_BackSpace), clearUrgents)
clearUrgents :: X ()
clearUrgents = withUrgents clearUrgents'

-- | X action that returns a list of currently urgent windows. You might use
-- it, or 'withUrgents', in your custom logHook, to display the workspaces that
-- contain urgent windows.
readUrgents :: X [Window]
readUrgents = XS.gets fromUrgents

-- | An HOF version of 'readUrgents', for those who prefer that sort of thing.
withUrgents :: ([Window] -> X a) -> X a
withUrgents f = readUrgents >>= f

-- | Cleanup urgency and reminders for windows that no longer exist.
cleanupStaleUrgents :: X ()
cleanupStaleUrgents = withWindowSet $ \ws -> do
    adjustUrgents (filter (`W.member` ws))
    adjustReminders (filter $ ((`W.member` ws) . window))

adjustUrgents :: ([Window] -> [Window]) -> X ()
adjustUrgents = XS.modify . onUrgents

type Interval = Rational

-- | An urgency reminder, as reified for 'RemindWhen'.
-- The last value is the countdown number, for 'Repeatedly'.
data Reminder = Reminder { timer     :: TimerId
                         , window    :: Window
                         , interval  :: Interval
                         , remaining :: Maybe Int
                         } deriving (Show,Read,Eq,Typeable)

instance ExtensionClass [Reminder] where
    initialValue = []
    extensionType = PersistentExtension

-- | Stores the list of urgency reminders.

readReminders :: X [Reminder]
readReminders = XS.get

adjustReminders :: ([Reminder] -> [Reminder]) -> X ()
adjustReminders = XS.modify


data WithUrgencyHook h = WithUrgencyHook h UrgencyConfig
    deriving (Read, Show)

-- | Change the _NET_WM_STATE property by applying a function to the list of atoms.
changeNetWMState :: Display -> Window -> ([CLong] -> [CLong]) -> X ()
changeNetWMState dpy w f = do
   wmstate <- getAtom "_NET_WM_STATE"
   wstate  <- fromMaybe [] <$> getProp32 wmstate w
   io $ changeProperty32 dpy w wmstate aTOM propModeReplace (f wstate)
   return ()

-- | Add an atom to the _NET_WM_STATE property.
addNetWMState :: Display -> Window -> Atom -> X ()
addNetWMState dpy w atom = changeNetWMState dpy w $ ((fromIntegral atom):)

-- | Remove an atom from the _NET_WM_STATE property.
removeNetWMState :: Display -> Window -> Atom -> X ()
removeNetWMState dpy w atom = changeNetWMState dpy w $ delete (fromIntegral atom)

-- | Get the _NET_WM_STATE propertly as a [CLong]
getNetWMState :: Window -> X [CLong]
getNetWMState w = do
    a_wmstate <- getAtom "_NET_WM_STATE"
    fromMaybe [] <$> getProp32 a_wmstate w


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
handleEvent :: UrgencyHook h => WithUrgencyHook h -> Event -> X ()
handleEvent wuh event =
    case event of
     -- WM_HINTS urgency flag
      PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = w } ->
          when (t == propertyNotify && a == wM_HINTS) $ withDisplay $ \dpy -> do
              WMHints { wmh_flags = flags } <- io $ getWMHints dpy w
              if (testBit flags urgencyHintBit) then markUrgent w else markNotUrgent w
      -- Window destroyed
      DestroyWindowEvent {ev_window = w} ->
          markNotUrgent w
      -- _NET_WM_STATE_DEMANDS_ATTENTION requested by client
      ClientMessageEvent {ev_event_display = dpy, ev_window = w, ev_message_type = t, ev_data = action:atoms} -> do
          a_wmstate <- getAtom "_NET_WM_STATE"
          a_da      <- getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"
          wstate    <- getNetWMState w
          let demandsAttention = fromIntegral a_da `elem` wstate
              remove = 0
              add    = 1
              toggle = 2
          when (t == a_wmstate && fromIntegral a_da `elem` atoms) $ do
            when (action == add || (action == toggle && not demandsAttention)) $ do
              markUrgent w
              addNetWMState dpy w a_da
            when (action == remove || (action == toggle && demandsAttention)) $ do
              markNotUrgent w
              removeNetWMState dpy w a_da
      _ ->
          mapM_ handleReminder =<< readReminders
      where handleReminder reminder = handleTimer (timer reminder) event $ reminderHook wuh reminder
            markUrgent w = do
                adjustUrgents (\ws -> if elem w ws then ws else w : ws)
                callUrgencyHook wuh w
                userCodeDef () =<< asks (logHook . config)
            markNotUrgent w = do
                adjustUrgents (delete w) >> adjustReminders (filter $ (w /=) . window)
                userCodeDef () =<< asks (logHook . config)

callUrgencyHook :: UrgencyHook h => WithUrgencyHook h -> Window -> X ()
callUrgencyHook (WithUrgencyHook hook UrgencyConfig { suppressWhen = sw, remindWhen = rw }) w =
    whenX (not <$> shouldSuppress sw w) $ do
        userCodeDef () $ urgencyHook hook w
        case rw of
            Repeatedly times int -> addReminder w int $ Just times
            Every int            -> addReminder w int Nothing
            Dont                 -> return ()

addReminder :: Window -> Rational -> Maybe Int -> X ()
addReminder w int times = do
    timerId <- startTimer int
    let reminder = Reminder timerId w int times
    adjustReminders (\rs -> if w `elem` map window rs then rs else reminder : rs)

reminderHook :: UrgencyHook h => WithUrgencyHook h -> Reminder -> X (Maybe a)
reminderHook (WithUrgencyHook hook _) reminder = do
    case remaining reminder of
        Just x | x > 0 -> remind $ Just (x - 1)
        Just _         -> adjustReminders $ delete reminder
        Nothing        -> remind Nothing
    return Nothing
  where remind remaining' = do userCode $ urgencyHook hook (window reminder)
                               adjustReminders $ delete reminder
                               addReminder (window reminder) (interval reminder) remaining'

shouldSuppress :: SuppressWhen -> Window -> X Bool
shouldSuppress sw w = elem w <$> suppressibleWindows sw

cleanupUrgents :: SuppressWhen -> X ()
cleanupUrgents sw = clearUrgents' =<< suppressibleWindows sw

-- | Clear urgency status of selected windows.
clearUrgents' :: [Window] -> X ()
clearUrgents' ws = do
    a_da <- getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"
    dpy <- withDisplay (\dpy -> return dpy)
    mapM_ (\w -> removeNetWMState dpy w a_da) ws
    adjustUrgents (\\ ws) >> adjustReminders (filter $ ((`notElem` ws) . window))

suppressibleWindows :: SuppressWhen -> X [Window]
suppressibleWindows Visible  = gets $ S.toList . mapped
suppressibleWindows OnScreen = gets $ W.index . windowset
suppressibleWindows Focused  = gets $ maybeToList . W.peek . windowset
suppressibleWindows Never    = return []

--------------------------------------------------------------------------------
-- Urgency Hooks

-- | The class definition, and some pre-defined instances.

class UrgencyHook h where
    urgencyHook :: h -> Window -> X ()

instance UrgencyHook (Window -> X ()) where
    urgencyHook = id

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
focusHook :: Window -> X ()
focusHook = urgencyHook FocusHook
data FocusHook = FocusHook deriving (Read, Show)

instance UrgencyHook FocusHook where
    urgencyHook _ _ = focusUrgent

-- | A hook that sets the border color of an urgent window.  The color
--   will remain until the next time the window gains or loses focus, at
--   which point the standard border color from the XConfig will be applied.
--   You may want to use suppressWhen = Never with this:
--
--   > withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "#ff0000" } urgencyConfig { suppressWhen = Never } ...
--
--   (This should be @urgentBorderColor@ but that breaks "XMonad.Layout.Decoration".
--   @borderColor@ breaks anyone using 'XPConfig' from "XMonad.Prompt".  We need to
--   think a bit more about namespacing issues, maybe.)

borderUrgencyHook :: String -> Window -> X ()
borderUrgencyHook = urgencyHook . BorderUrgencyHook
data BorderUrgencyHook = BorderUrgencyHook { urgencyBorderColor :: !String }
                       deriving (Read, Show)

instance UrgencyHook BorderUrgencyHook where
  urgencyHook BorderUrgencyHook { urgencyBorderColor = cs } w =
    withDisplay $ \dpy -> do
      c' <- io (initColor dpy cs)
      case c' of
        Just c -> setWindowBorderWithFallback dpy w cs c
        _      -> io $ hPutStrLn stderr $ concat ["Warning: bad urgentBorderColor "
                                                 ,show cs
                                                 ," in BorderUrgencyHook"
                                                 ]

-- | Flashes when a window requests your attention and you can't see it.
-- Defaults to a duration of five seconds, and no extra args to dzen.
-- See 'DzenUrgencyHook'.
dzenUrgencyHook :: DzenUrgencyHook
dzenUrgencyHook = DzenUrgencyHook { duration = seconds 5, args = [] }

-- | Spawn a commandline thing, appending the window id to the prefix string
-- you provide. (Make sure to add a space if you need it.) Do your crazy
-- xcompmgr thing.
spawnUrgencyHook :: String -> Window -> X ()
spawnUrgencyHook = urgencyHook . SpawnUrgencyHook
newtype SpawnUrgencyHook = SpawnUrgencyHook String deriving (Read, Show)

instance UrgencyHook SpawnUrgencyHook where
    urgencyHook (SpawnUrgencyHook prefix) w = spawn $ prefix ++ show w

-- | For debugging purposes, really.
stdoutUrgencyHook :: Window -> X ()
stdoutUrgencyHook = urgencyHook StdoutUrgencyHook
data StdoutUrgencyHook = StdoutUrgencyHook deriving (Read, Show)

instance UrgencyHook StdoutUrgencyHook where
    urgencyHook    _ w = io $ putStrLn $ "Urgent: " ++ show w

-- | urgencyhook such that windows on certain workspaces
-- never get urgency set.
--
-- Useful for scratchpad workspaces perhaps:
--
-- > main = xmonad (withUrgencyHook (filterUrgencyHook ["NSP", "SP"]) def)
filterUrgencyHook :: [WorkspaceId] -> Window -> X ()
filterUrgencyHook skips = filterUrgencyHook' $ maybe False (`elem` skips) <$> windowTag

-- | 'filterUrgencyHook' that takes a generic 'Query' to select which windows
-- should never be marked urgent.
filterUrgencyHook' :: Query Bool -> Window -> X ()
filterUrgencyHook' q w = whenX (runQuery q w) (clearUrgents' [w])

-- | Mark the given window urgent.
--
-- (The implementation is a bit hacky: send a _NET_WM_STATE ClientMessage to
-- ourselves. This is so that we respect the 'SuppressWhen' of the configured
-- urgency hooks.)
--
-- TODO: https://github.com/quchen/articles/blob/master/haskell-cpp-compatibility.md
askUrgent :: Window -> X ()
askUrgent w = withDisplay $ \dpy -> do
    rw <- asks theRoot
    a_wmstate <- getAtom "_NET_WM_STATE"
    a_da      <- getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"
    let state_add = 1
    let source_pager = 2
    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent' e w a_wmstate 32 [state_add, fi a_da, 0, source_pager]
        sendEvent dpy rw False (substructureRedirectMask .|. substructureNotifyMask) e

-- | Helper for 'ManageHook' that marks the window as urgent (unless
-- suppressed, see 'SuppressWhen'). Useful in
-- 'XMonad.Hooks.EwmhDesktops.activateHook' and also in combination with
-- "XMonad.Hooks.InsertPosition", "XMonad.Hooks.Focus".
doAskUrgent :: ManageHook
doAskUrgent = ask >>= \w -> liftX (askUrgent w) >> return mempty
