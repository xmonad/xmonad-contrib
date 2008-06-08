-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Scratchpad
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
  ,scratchpadManageHookDefault
  ,scratchpadManageHook
  ) where

import XMonad
import XMonad.Core
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)

import Control.Monad (filterM)

import qualified XMonad.StackSet as W


-- $usage
-- Bind a key to 'scratchpadSpawnAction'
-- Pressing it will spawn the terminal, or bring it to the current
-- workspace if it already exists.
-- Pressing the key with the terminal on the current workspace will 
-- send it to a hidden workspace called @SP@.
--
-- If you already have a workspace called @SP@, it will use that.
-- @SP@ will also appear in xmobar and dzen status bars. You can tweak your
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
-- The terminal application must support the @-title@ argument.
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
    scratchpadAction $ spawn $ terminal conf ++ " -title scratchpad"


-- | Action to pop up the terminal, with a directly specified terminal.
scratchpadSpawnActionTerminal :: String -- ^ Name of the terminal program
                                 -> X ()
scratchpadSpawnActionTerminal term = 
    scratchpadAction $ spawn $ term ++ " -title scratchpad"




-- The heart of the new summon/banish terminal. 
-- The logic is thus:
-- 1. if the scratchpad is on the current workspace, send it to the hidden one.
--    - if the scratchpad workspace doesn't exist yet, create it first.
-- 2. if the scratchpad is elsewhere, bring it here. 
scratchpadAction :: X () -> X ()
scratchpadAction action = withWindowSet $ \s -> do
  filterCurrent <- filterM (runQuery scratchpadQuery) 
                     ( (maybe [] W.integrate 
                        . W.stack 
                        . W.workspace 
                        . W.current) s)
  case filterCurrent of
    (x:_) -> do
      if null (filter ( (== scratchpadWorkspaceTag) . W.tag) (W.workspaces s))
         then addHiddenWorkspace scratchpadWorkspaceTag
         else return ()
      windows (W.shiftWin scratchpadWorkspaceTag x)
    []    -> do
      filterAll <- filterM (runQuery scratchpadQuery) (W.allWindows s)
      case filterAll of
        (x:_) -> windows (W.shiftWin (W.currentTag s) x)
        []    -> action -- run the provided action to spawn it.


-- factored out since it appears in several places
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "SP"

-- factored out since this is common to both the ManageHook and the action
scratchpadQuery :: Query Bool
scratchpadQuery = title =? "scratchpad"


-- | The ManageHook, with the default rectangle:
-- Half the screen wide, a quarter of the screen tall, centered.
scratchpadManageHookDefault :: ManageHook
scratchpadManageHookDefault = scratchpadManageHook scratchpadDefaultRect


-- | The ManageHook, with a user-specified StackSet.RationalRect,
-- eg.
--
-- > scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.25)
scratchpadManageHook :: W.RationalRect -- ^ User-specified screen rectangle.
                     -> ManageHook
scratchpadManageHook rect = scratchpadQuery --> doRectFloat rect


scratchpadDefaultRect :: W.RationalRect
scratchpadDefaultRect = W.RationalRect 0.25 0.375 0.5 0.25


