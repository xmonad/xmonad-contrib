{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicLog
-- Description :  Send information about xmonad's state to an X11 property or standard output.
-- Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- __Note:__ This module is a __compatibility wrapper__ for the following:
--
-- * "XMonad.Hooks.StatusBar"
-- * "XMonad.Hooks.StatusBar.PP"
--
-- DynamicLog API is frozen and users are encouraged to migrate to these
-- modern replacements.
--
-- /Original description and documentation follows:/
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.  DynamicLog
-- provides several drop-in logHooks for this purpose, as well as
-- flexible tools for specifying your own formatting.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicLog (
    -- * Usage
    -- $usage

    -- * Drop-in loggers
    xmobarProp,
    xmobar,
    statusBar,
    dzen,
    dzenWithFlags,
    dynamicLog,
    dynamicLogXinerama,

    xmonadPropLog,
    xmonadPropLog',
    xmonadDefProp,

    -- * Build your own formatter
    dynamicLogWithPP,
    dynamicLogString,
    PP(..), def,

    -- * Example formatters
    dzenPP, xmobarPP, sjanssenPP, byorgeyPP,

    -- * Formatting utilities
    wrap, pad, trim, shorten, shorten', shortenLeft, shortenLeft',
    xmobarColor, xmobarAction, xmobarBorder,
    xmobarRaw, xmobarStrip, xmobarStripTags,
    dzenColor, dzenEscape, dzenStrip, filterOutWsPP,

    -- * Internal formatting functions
    pprWindowSet,
    pprWindowSetXinerama,

  ) where

-- Useful imports

import XMonad

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.StatusBar

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.DynamicLog
--
-- If you just want a quick-and-dirty status bar with zero effort, try
-- the 'xmobar' or 'dzen' functions:
--
-- > main = xmonad =<< xmobar myConfig
-- >
-- > myConfig = def { ... }
--
-- There is also 'statusBar' if you'd like to use another status bar, or would
-- like to use different formatting options.  The 'xmobar', 'dzen', and
-- 'statusBar' functions are preferred over the other options listed below, as
-- they take care of all the necessary plumbing -- no shell scripting required!
--
-- Alternatively, you can choose among several default status bar formats
-- ('dynamicLog' or 'dynamicLogXinerama') by simply setting your logHook to the
-- appropriate function, for instance:
--
-- > main = xmonad $ def {
-- >    ...
-- >    logHook = dynamicLog
-- >    ...
-- >  }
--
-- For more flexibility, you can also use 'dynamicLogWithPP' and supply
-- your own pretty-printing format (by either defining one from scratch,
-- or customizing one of the provided examples).
-- For example:
--
-- >    -- use sjanssen's pretty-printer format, but with the sections
-- >    -- in reverse
-- >    logHook = dynamicLogWithPP $ sjanssenPP { ppOrder = reverse }
--
-- Note that setting the @logHook@ only sets up xmonad's output; you
-- are responsible for starting your own status bar program (e.g. dzen
-- or xmobar) and making sure xmonad's output is piped into it
-- appropriately, either by putting it in your @.xsession@ or similar
-- file, or by using @spawnPipe@ in your @main@ function, for example:
--
-- > import XMonad.Util.Run   -- for spawnPipe and hPutStrLn
-- >
-- > main = do
-- >     h <- spawnPipe "xmobar -options -foo -bar"
-- >     xmonad $ def {
-- >       ...
-- >       logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn h }
--
-- If you use @spawnPipe@, be sure to redefine the 'ppOutput' field of
-- your pretty-printer as in the example above; by default the status
-- will be printed to stdout rather than the pipe you create.
--
-- Even if you don't use a statusbar, you can still use
-- 'dynamicLogString' to show on-screen notifications in response to
-- some events. For example, to show the current layout when it
-- changes, you could make a keybinding to cycle the layout and
-- display the current status:
--
-- >    , ((mod1Mask, xK_a     ), sendMessage NextLayout >> (dynamicLogString myPP >>= xmessage))
--

------------------------------------------------------------------------

-- | Run xmonad with a dzen status bar with specified dzen
--   command line arguments.
--
-- > main = xmonad =<< dzenWithFlags flags myConfig
-- >
-- > myConfig = def { ... }
-- >
-- > flags = "-e onstart lower -w 800 -h 24 -ta l -fg #a8a3f7 -bg #3f3c6d"
--
-- This function works much in the same way as the 'dzen' function, only that it
-- can also be used to customize the arguments passed to dzen2, e.g changing the
-- default width and height of dzen2.
--
-- You should use this function only when the default 'dzen' function does not
-- serve your purpose.
--
dzenWithFlags :: LayoutClass l Window
              => String     -- ^ Flags to give to @dzen@
              -> XConfig l  -- ^ The base config
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzenWithFlags flags = statusBar ("dzen2 " ++ flags) dzenPP toggleStrutsKey

-- | Run xmonad with a dzen status bar set to some nice defaults.
--
-- > main = xmonad =<< dzen myConfig
-- >
-- > myConfig = def { ... }
--
-- This works pretty much the same as the 'xmobar' function.
--
dzen :: LayoutClass l Window
     => XConfig l  -- ^ The base config
     -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzen = dzenWithFlags flags
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e 'onstart=lower' -dock -w 400 -ta l -fg " ++ fg ++ " -bg " ++ bg

-- | This function works like 'xmobarProp', but uses pipes instead of
-- property-based logging.
xmobar :: LayoutClass l Window
       => XConfig l  -- ^ The base config
       -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = statusBar "xmobar" xmobarPP toggleStrutsKey

-- | Like 'statusBarProp', but uses pipes instead of property-based logging.
-- Only use this function if your status bar does not support reading from a
-- property of the root window.
statusBar :: LayoutClass l Window
          => String    -- ^ The command line to launch the status bar
          -> PP        -- ^ The pretty printing options
          -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ The desired key binding to toggle bar visibility
          -> XConfig l -- ^ The base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd pp k conf= do
  sb <- statusBarPipe cmd (pure pp)
  return $ withEasySB sb k conf

-- |
-- Helper function which provides ToggleStruts keybinding
--
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey = defToggleStrutsKey

-- | An example log hook, which prints status information to stdout in
-- the default format:
--
-- > 1 2 [3] 4 7 : full : title
--
-- That is, the currently populated workspaces, the current
-- workspace layout, and the title of the focused window.
--
-- To customize the output format, see 'dynamicLogWithPP'.
--
dynamicLog :: X ()
dynamicLog = dynamicLogWithPP def

-- |
-- Workspace logger with a format designed for Xinerama:
--
-- > [1 9 3] 2 7
--
-- where 1, 9, and 3 are the workspaces on screens 1, 2 and 3, respectively,
-- and 2 and 7 are non-visible, non-empty workspaces.
--
-- At the present time, the current layout and window title
-- are not shown.  The xinerama workspace format shown above can be (mostly) replicated
-- using 'dynamicLogWithPP' by setting 'ppSort' to /getSortByXineramaRule/ from
-- "XMonad.Util.WorkspaceCompare".  For example,
--
-- > def { ppCurrent = dzenColor "red" "#efebe7"
-- >     , ppVisible = wrap "[" "]"
-- >     , ppSort    = getSortByXineramaRule
-- >     }
dynamicLogXinerama :: X ()
dynamicLogXinerama = withWindowSet $ io . putStrLn . pprWindowSetXinerama

-- | Run xmonad with a property-based xmobar status bar set to some nice
-- defaults.
--
-- > main = xmonad $ xmobarProp myConfig
-- >
-- > myConfig = def { ... }
--
-- The intent is that the above config file should provide a nice
-- status bar with minimal effort. Note that you still need to configure
-- xmobar to use the @XMonadLog@ plugin instead of the default @StdinReader@,
-- see above.
--
-- If you wish to customize the status bar format at all, use the modernized
-- interface provided by the "XMonad.Hooks.StatusBar" and
-- "XMonad.Hooks.StatusBar.PP" modules instead.
--
-- The binding uses the "XMonad.Hooks.ManageDocks" module to automatically
-- handle screen placement for xmobar, and enables 'mod-b' for toggling
-- the menu bar.
xmobarProp :: LayoutClass l Window
           => XConfig l  -- ^ The base config
           -> XConfig (ModifiedLayout AvoidStruts l)
xmobarProp =
  withEasySB (statusBarProp "xmobar" (pure xmobarPP)) toggleStrutsKey
