{-# LANGUAGE FlexibleContexts, TypeApplications #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.StatusBar
-- Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides a new interface that replaces "XMonad.Hooks.DynamicLog",
-- by providing composoble and dynamic status bars. Supports property-based as well
-- as pipe-based status bars.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.StatusBar (
  -- * Usage
  -- $usage
  StatusBarConfig(..),
  makeStatusBar,
  makeStatusBar',

  -- * Available Configs
  -- $availableconfigs
  statusBarPipe,
  statusBarProp,
  statusBarPropTo,

  -- * Multiple Status Bars
  -- $multiple

  -- * Property Logging utilities
  xmonadPropLog,
  xmonadPropLog',
  xmonadDefProp,

  -- * Managing Status Bar Processes
  spawnStatusBarAndRemember,
  cleanupStatusBars,
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)

import qualified Data.Map        as M

import Foreign.C (CChar)

import XMonad

import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar.PP

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Hooks.StatusBar
-- >    import XMonad.Hooks.StatusBar.PP
--
-- The recommended way to use this module with xmobar, as well as any other
-- status bar that supports property logging (you can read more about X11
-- properties
-- [here](https://en.wikipedia.org/wiki/X_Window_System_core_protocol#Properties)
-- or
-- [here](https://tronche.com/gui/x/xlib/window-information/properties-and-atoms.html),
-- although you don't have to understand them in order to use the functions
-- below), is to use 'statusBarProp' with 'makeStatusBar', which takes care of
-- the necessary plumbing-no shell scripting required!
--
-- > main = do
-- >   mySB <- statusBarProp "xmobar" (pure myPP)
-- >   xmonad =<< makeStatusBar mySB myConf
--
-- which plays nice with other combinators that you might have already
-- in your config:
--
-- > main = do
-- >   mySB <- statusBarProp "xmobar" (pure myPP)
-- >   xmonad =<< (makeStatusBar mySB . ewmh . docks $ def {...})
--
-- You then have to tell your status bar to read from the @_XMONAD_LOG@ property
-- of the root window.  In the case of xmobar, this is simply achieved by using
-- the @XMonadLog@ plugin instead of @StdinReader@ in your @.xmobarrc@:
--
-- > Config { ...
-- >        , commands = [ Run XMonadLog, ... ]
-- >        , template = "%XMonadLog% }{ ..."
-- >        }
--
-- If you don't have an @.xmobarrc@, create it; the @XMonadLog@ plugin is not
-- part of the default xmobar configuration and your status bar will not show
-- otherwise!
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
-- It can be used in the same way as 'statusBarProp' above (for xmobar, you will now
-- use the @StdinReader@ plugin in your @.xmobarrc@).  Instead of writing to
-- a property, this function opens a pipe and makes the given status bar read
-- from that pipe.
-- Please be aware that this kind of setup is very bug-prone and hence is
-- discouraged: if anything goes wrong with the bar, xmonad will freeze.
--
-- If you do not want to use any of the "batteries included" functions above,
-- you can also add all of the necessary plumbing yourself (the source of
-- 'makeStatusBar' might come in handy here).
--
-- 'xmonadPropLog' allows you to write a string to the @_XMONAD_LOG@ property of
-- the root window.  Together with 'dynamicLogString', you can now simply set
-- your 'logHook' to the appropriate function, for instance
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
-- 'spawnStatusBarAndRemember' to start them and 'cleanupStatusBars' to kill
-- previously started ones.
--
-- Even if you don't use a statusbar, you can still use 'dynamicLogString' to
-- show on-screen notifications in response to some events. For example, to show
-- the current layout when it changes, you could make a keybinding to cycle the
-- layout and display the current status:
--
-- > ((mod1Mask, xK_a), sendMessage NextLayout >> (dynamicLogString myPP >>= \d->spawn $"xmessage "++d))
--
-- If you use a status bar that does not support reading from a property log
-- (like dzen), and you don't want to use the 'statusBar' function, you can,
-- again, also manually add all of the required components.
--
-- This works much like the property based solution above, just that you will
-- want to use 'dynamicLog' or 'dynamicLogXinerama' in place of 'xmonadPropLog'.
--
-- > main = xmonad $ def {
-- >    ...
-- >    , logHook = dynamicLog
-- >    ...
-- >    }
--
-- For more flexibility, you can also use 'dynamicLogWithPP' and supply your own
-- pretty-printing format (by either defining one from scratch, or customizing
-- one of the provided examples).  For example:
--
-- >    -- use sjanssen's pretty-printer format, but with the sections
-- >    -- in reverse
-- >    logHook = dynamicLogWithPP $ sjanssenPP { ppOrder = reverse }
--
-- Again, you will have to do all the necessary plumbing yourself.  In addition,
-- you are also responsible for creating a pipe for you status bar to read from:
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
-- 'ppOutput' field of your pretty-printer, as was done in the example above; by
-- default the status will be printed to stdout rather than the pipe you create.
--
-- The status bars are managed through the 'StatusBarConfig', which provides
-- a convenient abstraction over what a status bar is and how to manage it.
-- This modules provides how to create these status bar configs, and how to
-- incorporate them in your xmonad config: using 'makeStatusBar'
-- or 'makeStatusBar''.
--
-- The difference between 'makeStatusBar' and 'makeStatusBar'' is that 'makeStatusBar'
-- tries to stay out of your way, whereas 'makeStatusBar'' configures an
-- extra keybinding to toggle the status bars, and also applies the
-- 'avoidStruts' layout modifier as well as the 'docks' combinator.

-- | This datataype abstracts a status bar to provide a common interface
-- functions like 'statusBarPipe' or 'statusBarProp'. Once defined, a status
-- bar can be incorporated in 'XConfig' by using 'makeStatusBar' or
-- 'makeStatusBar'', which take care of the necessary plumbing.
data StatusBarConfig = StatusBarConfig  { sbLogHook     :: X ()
                                        -- ^ What and how to log to the status bar.
                                        , sbStartupHook :: X ()
                                        -- ^ How to start the status bar.
                                        , sbCleanupHook :: X ()
                                        -- ^ How to kill the status bar when xmonad is restarted.
                                        -- This is useful when the status bar is not started
                                        -- with a pipe.
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
makeStatusBar :: LayoutClass l Window
              => StatusBarConfig    -- ^ The status bar config
              -> XConfig l          -- ^ The base config
              -> IO (XConfig l)
makeStatusBar (StatusBarConfig lh sh ch) conf =
  return $ conf
    { logHook     = logHook conf *> lh
    , startupHook = startupHook conf *> ch *> sh
    }

-- | Like 'makeStatusBar', but takes an extra key to toggle struts. It also
-- applies the 'avoidStruts' layout modifier and the 'docks' combinator.
--
-- Using this function multiple times to combine status bars may result in
-- only one status bar working properly. See the section on using multiple
-- status bars for more details.
makeStatusBar' :: LayoutClass l Window
               => StatusBarConfig -- ^ The status bar config
               -> (XConfig Layout -> (KeyMask, KeySym))
                                  -- ^ The key binding
               -> XConfig l       -- ^ The base config
               -> IO (XConfig (ModifiedLayout AvoidStruts l))
makeStatusBar' sb k conf = do
  conf' <- makeStatusBar sb conf
  return $ docks $ conf' { layoutHook = avoidStruts (layoutHook conf')
                         , keys       = (<>) <$> keys' <*> keys conf'
                         }
  where keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- | Creates a 'StatusBarConfig' that uses property logging to @_XMONAD_LOG@, which
-- is set in 'xmonadDefProp'
statusBarProp :: String -- ^ The command line to launch the status bar
                    -> X PP   -- ^ The pretty printing options
                    -> IO StatusBarConfig
statusBarProp = statusBarPropTo xmonadDefProp

-- | Like 'statusBarProp', but lets you define the property
statusBarPropTo :: String -- ^ Property to write the string to
                      -> String -- ^ The command line to launch the status bar
                      -> X PP   -- ^ The pretty printing options
                      -> IO StatusBarConfig
statusBarPropTo prop cmd pp = pure def
    { sbLogHook     = xmonadPropLog' prop =<< dynamicLogString =<< pp
    , sbStartupHook = spawnStatusBarAndRemember cmd
    , sbCleanupHook = cleanupStatusBars
    }

-- | Like 'statusBarProp', but uses pipe-based logging instead.
statusBarPipe :: String -- ^ The command line to launch the status bar
                      -> X PP   -- ^ The pretty printing options
                      -> IO StatusBarConfig
statusBarPipe cmd xpp  = do
    h <- spawnPipe cmd
    return $ def { sbLogHook = xpp >>= \pp -> dynamicLogWithPP pp { ppOutput = hPutStrLn h } }

-- $multiple
-- A pattern that is often found in a lot of configs that want multiple status bars,
-- generally goes something like this:
--
-- > main = do
-- >   xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top"
-- >   xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom"
-- >   xmproc2 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"
-- >   xmonad $ def {
-- >     ...
-- >     , logHook = dynamicLogWithPP ppTop { ppOutput = hPutStrLn xmproc0 }
-- >              >> dynamicLogWithPP ppBottom { ppOutput = hPutStrLn xmproc1 }
-- >              >> dynamicLogWithPP pp1 { ppOutput = hPutStrLn xmproc2 }
-- >     ...
-- >   }
--
-- Which has a lot of boilerplate and is error-prone. By using the new interface, the
-- config becomes more declarative and there's much less room for errors. You use it
-- by creating the suitable status bar configs and combining them with '<>':
--
-- > main = do
-- >   xmobarTop    <- statusBarPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top"    (pure ppTop)
-- >   xmobarBottom <- statusBarPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom" (pure ppBottom)
-- >   xmobar1      <- statusBarPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"       (pure pp1)
-- >   xmonad =<< makeStatusBar (xmobarTop <> xmobarBottom <> xmobar1) myConfig
--
-- Or if you're feeling adventurous:
--
-- > myBars = map (uncurry statusBarPipe) [ ("xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top",    pure ppTop)
-- >                                              , ("xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom", pure ppBottom)
-- >                                              , ("xmobar -x 1 $HOME/.config/xmobar/xmobarrc1",       pure pp1) ]
-- > main = do
-- >   sbs <- sequence myBars
-- >   xmonad =<< makeStatusBar (mconcat sbs) myConfig
--
-- The above examples also work if the different status bars support different
-- logging methods: you could do mix property logging and logging via standard input.
-- One thing to keep in mind: if multiple bars read from the same property, their content
-- will be the same. If you want to use property-based logging with multiple bars,
-- they should read from different properties.


-- | The default property xmonad writes to. (@_XMONAD_LOG@).
xmonadDefProp :: String
xmonadDefProp = "_XMONAD_LOG"

-- | Write a string to the @_XMONAD_LOG@ property on the root window.
xmonadPropLog :: String -> X ()
xmonadPropLog = xmonadPropLog' xmonadDefProp

-- | Write a string to a property on the root window.  This property is of type
-- @UTF8_STRING@. The string must have been processed by 'encodeString'
-- ('dynamicLogString' does this).
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
newtype StatusBarPIDs = StatusBarPIDs { getPIDs :: [ProcessID] }
  deriving (Show, Read)

instance ExtensionClass StatusBarPIDs where
  initialValue = StatusBarPIDs []
  extensionType = PersistentExtension

-- | Kills the status bars started with 'spawnStatusBarAndRemember', and resets
-- the state. This could go for example at the beginning of the startupHook.
--
-- Concretely, this function sends a 'sigTERM' to the saved PIDs using
-- 'signalProcessGroup' to effectively terminate all processes, regardless
-- of how many were started by using  'spawnStatusBarAndRemember'.
--
-- There is one caveat to keep in mind: to keep the implementation simple;
-- no checks are executed before terminating the processes. This means: if the
-- started process dies for some reason, and enough time passes for the PIDs
-- to wrap around, this function might terminate another process that happens
-- to have the same PID. However, this isn't a typical usage scenario.
cleanupStatusBars :: X ()
cleanupStatusBars =
    getPIDs <$> XS.get
    >>= (io . mapM_ killPid)
    >> XS.put (StatusBarPIDs [])
   where
    killPid :: ProcessID -> IO ()
    killPid pidToKill = void $ try @SomeException (signalProcessGroup sigTERM pidToKill)

-- | Spawns a status bar and saves its PID. This is useful when the status bars
-- should be restarted with xmonad. Use this in combination with 'cleanupStatusBars'.
--
-- Note: in some systems, multiple processes might start, even though one command is
-- provided. This means the first PID, of the group leader, is saved.
spawnStatusBarAndRemember :: String -- ^ The command used to spawn the status bar
                          -> X ()
spawnStatusBarAndRemember cmd = do
  newPid <- spawnPID cmd
  XS.modify (StatusBarPIDs . (newPid :) . getPIDs)
