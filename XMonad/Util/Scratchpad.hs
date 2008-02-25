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
-- A tool like detach (<http://detach.sourceforge.net>) turns it
-- into a launchpad for X apps.
--
-- By default, your xmonad terminal is used, and mod+s is the hotkey.
-- The default ManageHook uses a centered, half-screen-wide, 
-- quarter-screen-tall window.
-- The key, position and size are configurable.
--
-- The terminal application must support the @-title@ argument.
-- Known supported terminals: rxvt, rxvt-unicode, xterm.
-- Most others are likely to follow the lead set by xterm. 
-- 
-- Add the following to your xmonad.hs keybindings to use the default mod+s:
--
-- > scratchpadSpawnDefault conf
--
-- Or specify your own key binding, with the action:
--
-- > scratchpadSpawnAction conf
-- 
-- And add one of the @scratchpadManageHook*@s to your ManageHook list.
-- The default rectangle is half the screen wide and a quarter of the
-- screen tall, centered.
-- 
-----------------------------------------------------------------------------

module XMonad.Util.Scratchpad (
  scratchpadSpawnDefault
  ,scratchpadSpawnAction
  ,scratchpadManageHookDefault
  ,scratchpadManageHook
  ) where

import XMonad
import XMonad.Core
import XMonad.Hooks.ManageHelpers (doRectFloat)
import qualified XMonad.StackSet



-- | Complete key binding. Pops up the terminal on mod+s.
scratchpadSpawnDefault :: XConfig Layout -- ^ The configuration, to retrieve terminal and modMask
                       -> ((KeyMask, KeySym), X ())
scratchpadSpawnDefault conf = ((modMask conf, xK_s), scratchpadSpawnAction conf)


-- | Action to pop up the terminal, for the user to bind to a custom key.
scratchpadSpawnAction :: XConfig Layout -- ^ The configuration, to retrieve the terminal
                      -> X ()
scratchpadSpawnAction conf = spawn $ terminal conf ++ " -title scratchpad"



-- | The ManageHook, with the default rectangle:
-- Half the screen wide, a quarter of the screen tall, centered.
scratchpadManageHookDefault :: ManageHook
scratchpadManageHookDefault = scratchpadManageHook scratchpadDefaultRect


-- | The ManageHook, with a user-specified StackSet.RationalRect.
scratchpadManageHook :: XMonad.StackSet.RationalRect -- ^ User-specified screen rectangle.
                     -> ManageHook
scratchpadManageHook rect = title =? "scratchpad" --> doRectFloat rect


scratchpadDefaultRect :: XMonad.StackSet.RationalRect
scratchpadDefaultRect = XMonad.StackSet.RationalRect 0.25 0.375 0.5 0.25


