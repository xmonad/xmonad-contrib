-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Scratchpad
-- Description :  Very handy hotkey-launched toggleable floating terminal window.
-- Copyright   :  (c) Braden Shepherdson 2008
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Very handy hotkey-launched floating terminal window.
--
-----------------------------------------------------------------------------

module XMonad.Util.Scratchpad (
  -- * Usage
  -- $usage
  scratchpadSpawnAction
  ,scratchpadSpawnActionTerminal
  ,scratchpadSpawnActionCustom
  ,scratchpadManageHookDefault
  ,scratchpadManageHook
  ,scratchpadFilterOutWorkspace
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)


-- $usage
-- Bind a key to 'scratchpadSpawnAction'
-- Pressing it will spawn the terminal, or bring it to the current
-- workspace if it already exists.
-- Pressing the key with the terminal on the current workspace will
-- send it to a hidden workspace called @NSP@.
--
-- If you already have a workspace called @NSP@, it will use that.
-- @NSP@ will also appear in xmobar and dzen status bars. You can tweak your
-- @dynamicLog@ settings to filter it out if you like.
--
-- A tool like detach (<http://detach.sourceforge.net>) turns it
-- into a launchpad for X apps.
--
-- By default, your xmonad terminal is used.
-- The default ManageHook uses a centered, half-screen-wide,
-- quarter-screen-tall window.
-- The key, position and size are configurable.
--
-- The terminal application must support the @-name@ argument.
-- Known supported terminals: rxvt, rxvt-unicode, xterm.
-- Most others are likely to follow the lead set by xterm.
--
-- Bind the following to a key in your xmonad.hs keybindings:
--
-- > scratchpadSpawnAction conf
--
-- Where @conf@ is the configuration.
--
-- And add one of the @scratchpadManageHook*@s to your ManageHook list.
-- The default rectangle is half the screen wide and a quarter of the
-- screen tall, centered.
--



-- | Action to pop up the terminal, for the user to bind to a custom key.
scratchpadSpawnAction :: XConfig l -- ^ The configuration, to retrieve the terminal
                      -> X ()
scratchpadSpawnAction conf =
    scratchpadSpawnActionCustom $ terminal conf ++ " -name scratchpad"


-- | Action to pop up the terminal, with a directly specified terminal.
scratchpadSpawnActionTerminal :: String -- ^ Name of the terminal program
                                 -> X ()
scratchpadSpawnActionTerminal term =
    scratchpadSpawnActionCustom $ term ++ " -name scratchpad"


-- | Action to pop up any program with the user specifying how to set
--   its resource to \"scratchpad\". For example, with gnome-terminal:
--
-- > scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad"
scratchpadSpawnActionCustom :: String -- ^ Command to spawn a program with resource \"scratchpad\"
                                 -> X ()
scratchpadSpawnActionCustom c = namedScratchpadAction [NS "scratchpad" c scratchpadQuery nonFloating] "scratchpad"

-- factored out since this is common to both the ManageHook and the action
scratchpadQuery :: Query Bool
scratchpadQuery = resource =? "scratchpad"


-- | The ManageHook, with the default rectangle:
-- Half the screen wide, a quarter of the screen tall, centered.
scratchpadManageHookDefault :: ManageHook
scratchpadManageHookDefault = namedScratchpadManageHook [NS "" "" scratchpadQuery (customFloating scratchpadDefaultRect)]


-- | The ManageHook, with a user-specified StackSet.RationalRect,
--   e.g., for a terminal 4/10 of the screen width from the left, half
--   the screen height from the top, and 6/10 of the screen width by
--   3/10 the screen height, use:
--
-- > scratchpadManageHook (W.RationalRect 0.4 0.5 0.6 0.3)
scratchpadManageHook :: W.RationalRect -- ^ User-specified screen rectangle.
                     -> ManageHook
scratchpadManageHook rect = namedScratchpadManageHook [NS "" "" scratchpadQuery (customFloating rect)]


-- | Transforms a workspace list containing the SP workspace into one that
-- doesn't contain it. Intended for use with 'logHook's (see
-- 'XMonad.Hooks.StatusBar.PP.filterOutWsPP') and "XMonad.Hooks.EwmhDesktops"
-- (see 'XMonad.Hooks.EwmhDesktops.addEwmhWorkspaceSort').
scratchpadFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
scratchpadFilterOutWorkspace = filterOutWs [scratchpadWorkspaceTag]


scratchpadDefaultRect :: W.RationalRect
scratchpadDefaultRect = W.RationalRect 0.25 0.375 0.5 0.25
