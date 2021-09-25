{-# LANGUAGE FlexibleContexts, TypeApplications, TupleSections  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.StatusBar
-- Description :  Composable and dynamic status bars.
-- Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.
--
-- This module provides a composable interface for (re)starting these status
-- bars and logging to them, either using pipes or X properties. There's also
-- "XMonad.Hooks.StatusBar.PP" which provides an abstraction and some
-- utilities for customization what is logged to a status bar. Together, these
-- are a modern replacement for "XMonad.Hooks.DynamicLog", which is now just a
-- compatibility wrapper.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.StatusBar (
  -- * Usage
  -- $usage
  StatusBarConfig(..),
  withSB,
  withEasySB,
  defToggleStrutsKey,

  -- * Available Configs
  -- $availableconfigs
  statusBarProp,
  statusBarPropTo,
  statusBarGeneric,
  statusBarPipe,

  -- * Multiple Status Bars
  -- $multiple

  -- * Dynamic Status Bars
  -- $dynamic
  dynamicSBs,
  dynamicEasySBs,

  -- * Property Logging utilities
  xmonadPropLog,
  xmonadPropLog',
  xmonadDefProp,

  -- * Managing status bar Processes
  -- $sbprocess
  spawnStatusBar,
  killStatusBar,
  killAllStatusBars,
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.Map as M
import System.IO (hClose)
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)

import Foreign.C (CChar)

import XMonad
import XMonad.Prelude

import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.StatusBar
-- > import XMonad.Hooks.StatusBar.PP
--
-- The easiest way to use this module with xmobar, as well as any other
-- status bar that supports property logging, is to use 'statusBarProp'
-- with 'withEasySB'; these take care of the necessary plumbing:
--
-- > mySB = statusBarProp "xmobar" (pure xmobarPP)
-- > main = xmonad $ withEasySB mySB defToggleStrutsKey def
--
-- You can read more about X11 properties
-- [here](https://en.wikipedia.org/wiki/X_Window_System_core_protocol#Properties)
-- or
-- [here](https://tronche.com/gui/x/xlib/window-information/properties-and-atoms.html),
-- although you don't have to understand them in order to use the functions
-- mentioned above.
--
-- Most users will, however, want to customize the logging and integrate it
-- into their existing custom xmonad configuration. The 'withSB'
-- function is more appropriate in this case: it doesn't touch your
-- keybindings, layout modifiers, or event hooks; instead, you're expected
-- to configure "XMonad.Hooks.ManageDocks" yourself. Here's what that might
-- look like:
--
-- > mySB = statusBarProp "xmobar" (pure myPP)
-- > main = xmonad . withSB mySB . ewmh . docks $ def {...}
--
-- You then have to tell your status bar to read from the @_XMONAD_LOG@ property
-- of the root window.  In the case of xmobar, this is achieved by simply using
-- the @XMonadLog@ plugin instead of @StdinReader@ in your @.xmobarrc@:
--
-- > Config { ...
-- >        , commands = [ Run XMonadLog, ... ]
-- >        , template = "%XMonadLog% }{ ..."
-- >        }
--
-- If you don't have an @.xmobarrc@, create it; the @XMonadLog@ plugin is not
-- part of the default xmobar configuration and your status bar will not show
-- workspace information otherwise!
--
-- With 'statusBarProp', you need to use property logging. Make sure the
-- status bar you use supports reading a property string from the root window,
-- or use some kind of wrapper that reads the property and pipes it into the
-- bar (e.g. @xmonadpropread | dzen2@, see @scripts/xmonadpropread.hs@). The
-- default property is @_XMONAD_LOG@, which is conveniently saved in 'xmonadDefProp'.
-- You can use another property by using the function 'statusBarPropTo'.
--
-- If your status bar does not support property-based logging, you may also try
-- 'statusBarPipe'.
-- It can be used in the same way as 'statusBarProp' above (for xmobar, you now
-- have to use the @StdinReader@ plugin in your @.xmobarrc@).  Instead of
-- writing to a property, this function opens a pipe and makes the given status
-- bar read from that pipe.
-- Please be aware that this kind of setup is very bug-prone and hence is
-- discouraged: if anything goes wrong with the bar, xmonad will freeze!
--
-- Also note that 'statusBarPipe' returns 'IO StatusBarConfig', so
-- you need to evaluate it before passing it to 'withSB' or 'withEasySB':
--
-- > main = do
-- >   mySB <- statusBarPipe "xmobar" (pure myPP)
-- >   xmonad $ withSB mySB myConf


-- $plumbing
-- If you do not want to use any of the "batteries included" functions above,
-- you can also add all of the necessary plumbing yourself (the source of
-- 'withSB' might come in handy here).
--
-- 'xmonadPropLog' allows you to write a string to the @_XMONAD_LOG@ property of
-- the root window.  Together with 'dynamicLogString', you can now simply set
-- your 'logHook' to the appropriate function; for instance:
--
-- > main = xmonad $ def {
-- >    ...
-- >    , logHook = xmonadPropLog =<< dynamicLogString myPP
-- >    ...
-- >    }
--
-- If you want to define your own property name, use 'xmonadPropLog'' instead of
-- 'xmonadPropLog'.
--
-- If you just want to use the default pretty-printing format, you can replace
-- @myPP@ with 'def' in the above 'logHook'.
--
-- Note that setting 'logHook' only sets up xmonad's output; you are
-- responsible for starting your own status bar program and making sure it reads
-- from the property that xmonad writes to.  To start your bar, simply put it
-- into your 'startupHook'.  You will also have also have to add 'docks' and
-- 'avoidStruts' to your config.  Putting all of this together would look
-- something like
--
-- > import XMonad.Util.SpawnOnce (spawnOnce)
-- > import XMonad.Hooks.ManageDocks (avoidStruts, docks)
-- >
-- > main = do
-- >     xmonad $ docks $ def {
-- >       ...
-- >       , logHook     = xmonadPropLog =<< dynamicLogString myPP
-- >       , startupHook = spawnOnce "xmobar"
-- >       , layoutHook  = avoidStruts myLayout
-- >       ...
-- >       }
-- > myPP = def { ... }
-- > myLayout = ...
--
-- If you want a keybinding to toggle your bar, you will also need to add this
-- to the rest of your keybindings.
--
-- The above has the problem that xmobar will not get restarted whenever you
-- restart xmonad ('XMonad.Util.SpawnOnce.spawnOnce' will simply prevent your
-- chosen status bar from spawning again). Using 'statusBarProp', however, takes
-- care of the necessary plumbing /and/ keeps track of the started status bars, so
-- they can be correctly restarted with xmonad. This is achieved using
-- 'spawnStatusBar' to start them and 'killStatusBar' to kill
-- previously started bars.
--
-- Even if you don't use a status bar, you can still use 'dynamicLogString' to
-- show on-screen notifications in response to some events. For example, to show
-- the current layout when it changes, you could make a keybinding to cycle the
-- layout and display the current status:
--
-- > ((mod1Mask, xK_a), sendMessage NextLayout >> (dynamicLogString myPP >>= xmessage))
--
-- If you use a status bar that does not support reading from a property
-- (like dzen), and you don't want to use the 'statusBar' function, you can,
-- again, also manually add all of the required components, like this:
--
-- > import XMonad.Util.Run (hPutStrLn, spawnPipe)
-- >
-- > main = do
-- >     h <- spawnPipe "dzen2 -options -foo -bar"
-- >     xmonad $ def {
-- >       ...
-- >       , logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn h }
-- >       ...
-- >       }
--
-- In the above, note that if you use @spawnPipe@ you need to redefine the
-- 'ppOutput' field of your pretty-printer; by default the status will be
-- printed to stdout rather than the pipe you create. This was meant to be
-- used together with running xmonad piped to a status bar like so: @xmonad |
-- dzen2@, and is what the old 'XMonad.Hooks.DynamicLog.dynamicLog' assumes,
-- but it isn't recommended in modern setups. Applications launched from
-- xmonad inherit its stdout and stderr, and will print their own garbage to
-- the status bar.


-- | This datataype abstracts a status bar to provide a common interface
-- functions like 'statusBarPipe' or 'statusBarProp'. Once defined, a status
-- bar can be incorporated in 'XConfig' by using 'withSB' or
-- 'withEasySB', which take care of the necessary plumbing.
data StatusBarConfig = StatusBarConfig  { sbLogHook     :: X ()
                                        -- ^ What and how to log to the status bar.
                                        , sbStartupHook :: X ()
                                        -- ^ How to start the status bar.
                                        , sbCleanupHook :: X ()
                                        -- ^ How to kill the status bar.
                                        }

instance Semigroup StatusBarConfig where
    StatusBarConfig l s c <> StatusBarConfig l' s' c' =
      StatusBarConfig (l <> l') (s <> s') (c <> c')

instance Monoid StatusBarConfig where
    mempty = StatusBarConfig mempty mempty mempty

-- | Per default, all the hooks do nothing.
instance Default StatusBarConfig where
    def = mempty

-- | Incorporates a 'StatusBarConfig' into an 'XConfig' by taking care of the
-- necessary plumbing (starting, restarting and logging to it).
--
-- Using this function multiple times to combine status bars may result in
-- only one status bar working properly. See the section on using multiple
-- status bars for more details.
withSB :: LayoutClass l Window
       => StatusBarConfig    -- ^ The status bar config
       -> XConfig l          -- ^ The base config
       -> XConfig l
withSB (StatusBarConfig lh sh ch) conf = conf
    { logHook     = logHook conf *> lh
    , startupHook = startupHook conf *> ch *> sh
    }

-- | Like 'withSB', but takes an extra key to toggle struts. It also
-- applies the 'avoidStruts' layout modifier and the 'docks' combinator.
--
-- Using this function multiple times to combine status bars may result in
-- only one status bar working properly. See the section on using multiple
-- status bars for more details.
withEasySB :: LayoutClass l Window
           => StatusBarConfig -- ^ The status bar config
           -> (XConfig Layout -> (KeyMask, KeySym))
                              -- ^ The key binding
           -> XConfig l       -- ^ The base config
           -> XConfig (ModifiedLayout AvoidStruts l)
withEasySB sb k conf = docks . withSB sb $ conf
    { layoutHook = avoidStruts (layoutHook conf)
    , keys       = (<>) <$> keys' <*> keys conf
    }
  where
    k' conf' = case k conf' of
        (0, 0) ->
            -- This usually means the user passed 'def' for the keybinding
            -- function, and is otherwise meaningless to harmful depending on
            -- whether 383ffb7 has been applied to xmonad or not. So do what
            -- they probably intend.
            --
            -- A user who wants no keybinding function should probably use
            -- 'withSB' instead, especially since NoSymbol didn't do anything
            -- sane before 383ffb7. ++bsa
            defToggleStrutsKey conf'
        key -> key
    keys' = (`M.singleton` sendMessage ToggleStruts) . k'

-- | Default @mod-b@ key binding for 'withEasySB'
defToggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
defToggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

-- | Creates a 'StatusBarConfig' that uses property logging to @_XMONAD_LOG@, which
-- is set in 'xmonadDefProp'
statusBarProp :: String -- ^ The command line to launch the status bar
              -> X PP   -- ^ The pretty printing options
              -> StatusBarConfig
statusBarProp = statusBarPropTo xmonadDefProp

-- | Like 'statusBarProp', but lets you define the property
statusBarPropTo :: String -- ^ Property to write the string to
                -> String -- ^ The command line to launch the status bar
                -> X PP   -- ^ The pretty printing options
                -> StatusBarConfig
statusBarPropTo prop cmd pp = statusBarGeneric cmd $
    xmonadPropLog' prop =<< dynamicLogString =<< pp

-- | A generic 'StatusBarConfig' that launches a status bar but takes a
-- generic @X ()@ logging function instead of a 'PP'. This has several uses:
--
-- * With 'xmonadPropLog' or 'xmonadPropLog'' in the logging function, a
--   custom non-'PP'-based logger can be used for logging into an @xmobar@.
--
-- * With 'mempty' as the logging function, it's possible to manage a status
--   bar that reads information from EWMH properties like @taffybar@.
--
-- * With 'mempty' as the logging function, any other dock like @trayer@ or
--   @stalonetray@ can be managed by this module.
statusBarGeneric :: String -- ^ The command line to launch the status bar
                 -> X ()   -- ^ What and how to log to the status bar ('sbLogHook')
                 -> StatusBarConfig
statusBarGeneric cmd lh = def
    { sbLogHook     = lh
    , sbStartupHook = spawnStatusBar cmd
    , sbCleanupHook = killStatusBar cmd
    }

-- | Like 'statusBarProp', but uses pipe-based logging instead.
statusBarPipe :: String -- ^ The command line to launch the status bar
              -> X PP   -- ^ The pretty printing options
              -> IO StatusBarConfig
statusBarPipe cmd xpp = do
    hRef <- newIORef Nothing
    return $ def
        { sbStartupHook = io (writeIORef hRef . Just =<< spawnPipe cmd)
        , sbLogHook     = do
              h' <- io (readIORef hRef)
              whenJust h' $ \h -> io . hPutStrLn h =<< dynamicLogString =<< xpp
        , sbCleanupHook = io
                          $   readIORef hRef
                          >>= (`whenJust` hClose)
                          >>  writeIORef hRef Nothing
        }


-- $multiple
-- 'StatusBarConfig' is a 'Monoid', which means that multiple status bars can
-- be combined together using '<>' or 'mconcat' and passed to 'withSB'.
--
-- Here's an example of what such declarative configuration of multiple status
-- bars may look like:
--
-- > -- Make sure to setup the xmobar configs accordingly
-- > xmobarTop    = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobarrc_top"    (pure ppTop)
-- > xmobarBottom = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 0 ~/.config/xmobar/xmobarrc_bottom" (pure ppBottom)
-- > xmobar1      = statusBarPropTo "_XMONAD_LOG_3" "xmobar -x 1 ~/.config/xmobar/xmobarrc1"       (pure pp1)
-- >
-- > main = xmonad $ withSB (xmobarTop <> xmobarBottom <> xmobar1) myConfig
--
-- And here is an example of the related xmobar configuration for the multiple
-- status bars mentioned above:
--
-- > xmobarrc_top
-- > Config { ...
-- >        , commands = [ Run XPropertyLog "_XMONAD_LOG_1", ... ]
-- >        , template = "%_XMONAD_LOG_1% }{ ..."
-- >        }
--
-- The above example also works if the different status bars support different
-- logging methods: you could mix property logging and logging via pipes.
-- One thing to keep in mind is that if multiple bars read from the same
-- property, their content will be the same. If you want to use property-based
-- logging with multiple bars, they should read from different properties.
--
-- "XMonad.Util.Loggers" includes loggers that can be bound to specific screens,
-- like 'logCurrentOnScreen', that might be useful with multiple screens.
--
-- Long-time xmonad users will note that the above config is equivalent to
-- the following less robust and more verbose configuration that they might
-- find in their old configs:
--
-- > main = do
-- >   -- do not use this, this is an example of a deprecated config
-- >   xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc_top"
-- >   xmproc1 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc_bottom"
-- >   xmproc2 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc1"
-- >   xmonad $ def {
-- >     ...
-- >     , logHook = dynamicLogWithPP ppTop { ppOutput = hPutStrLn xmproc0 }
-- >              >> dynamicLogWithPP ppBottom { ppOutput = hPutStrLn xmproc1 }
-- >              >> dynamicLogWithPP pp1 { ppOutput = hPutStrLn xmproc2 }
-- >     ...
-- >   }
--
-- By using the new interface, the config becomes more declarative and there's
-- less room for errors.
--
-- The only *problem* now is that the status bars will not be updated when your screen
-- configuration changes (by plugging in a monitor, for example). Check the section
-- on dynamic status bars for how to do that.

-- $dynamic
-- Using multiple status bars by just combining them with '<>' works well
-- as long as the screen configuration does not change often. If it does,
-- you should use 'dynamicSBs': by providing a function that creates
-- status bars, it takes care of setting up the event hook, the log hook
-- and the startup hook necessary to make the status bars, well, dynamic.
--
-- > xmobarTop    = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobarrc_top"    (pure ppTop)
-- > xmobarBottom = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 0 ~/.config/xmobar/xmobarrc_bottom" (pure ppBottom)
-- > xmobar1      = statusBarPropTo "_XMONAD_LOG_3" "xmobar -x 1 ~/.config/xmobar/xmobarrc1"       (pure pp1)
-- >
-- > barSpawner :: ScreenId -> IO StatusBarConfig
-- > barSpawner 0 = pure $ xmobarTop <> xmobarBottom -- two bars on the main screen
-- > barSpawner 1 = pure $ xmobar1
-- > barSpawner _ = mempty -- nothing on the rest of the screens
-- >
-- > main = xmonad $ dynamicSBs barSpawner (def { ... })
--
-- Make sure you specify which screen to place the status bar on (in xmobar,
-- this is achieved by the @-x@ argument). In addition to making sure that your
-- status bar lands where you intended it to land, the commands are used
-- internally to keep track of the status bars.
--
-- Note also that this interface can be used with one screen, or if
-- the screen configuration doesn't change.

newtype ActiveSBs = ASB {getASBs :: [(ScreenId,  StatusBarConfig)]}

instance ExtensionClass ActiveSBs where
  initialValue = ASB []

-- | Given a function to create status bars, 'dynamicSBs'
-- adds the dynamic status bar capabilities to the config.
-- For a version of this function that applies 'docks' and
-- 'avoidStruts', check 'dynamicEasySBs'.
--
-- Heavily inspired by "XMonad.Hooks.DynamicBars"
dynamicSBs :: (ScreenId -> IO StatusBarConfig) -> XConfig l -> XConfig l
dynamicSBs f conf = addAfterRescreenHook (updateSBs f) $ conf
  { startupHook = startupHook conf >> killAllStatusBars >> updateSBs f
  , logHook     = logHook conf >> logSBs
  }

-- | Like 'dynamicSBs', but applies 'docks' to the
-- resulting config and adds 'avoidStruts' to the
-- layout.
dynamicEasySBs :: LayoutClass l Window
               => (ScreenId -> IO StatusBarConfig)
               -> XConfig l
               -> XConfig (ModifiedLayout AvoidStruts l)
dynamicEasySBs f conf =
  docks . dynamicSBs f $ conf { layoutHook = avoidStruts (layoutHook conf) }

-- | Given the function to create status bars, update
-- the status bars by killing those that shouldn't be
-- visible anymore and creates any missing status bars
updateSBs :: (ScreenId -> IO StatusBarConfig) -> X ()
updateSBs f = do
  actualScreens    <- withWindowSet $ return . map W.screen . W.screens
  (toKeep, toKill) <-
    partition ((`elem` actualScreens) . fst) . getASBs <$> XS.get
  -- Kill the status bars
  cleanSBs (map snd toKill)
  -- Create new status bars if needed
  let missing = actualScreens \\ map fst toKeep
  added <- io $ traverse (\s -> (s,) <$> f s) missing
  traverse_ (sbStartupHook . snd) added
  XS.put (ASB (toKeep ++ added))

-- | Run 'sbLogHook' for the saved 'StatusBarConfig's
logSBs :: X ()
logSBs = XS.get >>= traverse_ (sbLogHook . snd) . getASBs

-- | Kill the given 'StatusBarConfig's from the given
-- list
cleanSBs :: [StatusBarConfig] -> X ()
cleanSBs = traverse_ sbCleanupHook

-- | The default property xmonad writes to. (@_XMONAD_LOG@).
xmonadDefProp :: String
xmonadDefProp = "_XMONAD_LOG"

-- | Write a string to the @_XMONAD_LOG@ property on the root window.
xmonadPropLog :: String -> X ()
xmonadPropLog = xmonadPropLog' xmonadDefProp

-- | Write a string to a property on the root window.  This property is of type
-- @UTF8_STRING@.
xmonadPropLog' :: String  -- ^ Property name
               -> String  -- ^ Message to be written to the property
               -> X ()
xmonadPropLog' prop msg = do
    d <- asks display
    r <- asks theRoot
    xlog <- getAtom prop
    ustring <- getAtom "UTF8_STRING"
    io $ changeProperty8 d r xlog ustring propModeReplace (encodeCChar msg)
 where
    encodeCChar :: String -> [CChar]
    encodeCChar = map fromIntegral . UTF8.encode


-- This newtype wrapper, together with the ExtensionClass instance make use of
-- the extensible state to save the PIDs bewteen xmonad restarts.
newtype StatusBarPIDs = StatusBarPIDs { getPIDs :: M.Map String ProcessID }
  deriving (Show, Read)

instance ExtensionClass StatusBarPIDs where
  initialValue = StatusBarPIDs mempty
  extensionType = PersistentExtension

-- | Kills the status bar started with 'spawnStatusBar' using the given command
-- and resets the state. This could go for example at the beginning of the
-- startupHook, to kill the status bars that need to be restarted.
--
-- Concretely, this function sends a 'sigTERM' to the saved PIDs using
-- 'signalProcessGroup' to effectively terminate all processes, regardless
-- of how many were started by using  'spawnStatusBar'.
--
-- There is one caveat to keep in mind: to keep the implementation simple;
-- no checks are executed before terminating the processes. This means: if the
-- started process dies for some reason, and enough time passes for the PIDs
-- to wrap around, this function might terminate another process that happens
-- to have the same PID. However, this isn't a typical usage scenario.
killStatusBar :: String -- ^ The command used to start the status bar
                 -> X ()
killStatusBar cmd = do
    XS.gets (M.lookup cmd . getPIDs) >>= flip whenJust (io . killPid)
    XS.modify (StatusBarPIDs . M.delete cmd . getPIDs)

killPid :: ProcessID -> IO ()
killPid pidToKill = void $ try @SomeException (signalProcessGroup sigTERM pidToKill)

-- | Spawns a status bar and saves its PID together with the commands that was
-- used to start it. This is useful when the status bars should be restarted
-- with xmonad. Use this in combination with 'killStatusBar'.
--
-- Note: in some systems, multiple processes might start, even though one command is
-- provided. This means the first PID, of the group leader, is saved.
spawnStatusBar :: String -- ^ The command used to spawn the status bar
               -> X ()
spawnStatusBar cmd = do
  newPid <- spawnPID cmd
  XS.modify (StatusBarPIDs . M.insert cmd newPid . getPIDs)

-- | Kill all status bars started with 'spawnStatusBar'. Note the
-- caveats in 'cleanupStatusBar'
killAllStatusBars :: X ()
killAllStatusBars =
  XS.gets (M.elems . getPIDs) >>= io . traverse_ killPid >> XS.put (StatusBarPIDs mempty)
