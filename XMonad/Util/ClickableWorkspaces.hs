-------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ClickableWorkspaces
-- Copyright   :  (c) Geoff deRosenroll <geoffderosenroll@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Geoff deRosenroll <geoffderosenroll@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides @clickablePP@, which when applied to the PP pretty-printer used by
-- the "XMonad.Hooks.DynamicLog" hook, will make the workspace tags clickable in
-- XMobar (for switching focus).
--
-----------------------------------------------------------------------------

module XMonad.Util.ClickableWorkspaces (
  -- * Usage
  -- $usage
  clickablePP,
  clickableWrap,
  ) where

import Control.Monad ((>=>))
import Data.Functor ((<&>))

import XMonad
import XMonad.Hooks.DynamicLog (xmobarAction, PP(..))
import XMonad.Util.WorkspaceCompare (getWsIndex)
import qualified XMonad.StackSet as W

-- $usage
-- However you have set up your PP, apply @clickablePP@ to it, and bind the result
-- to "XMonad.Hooks.DynamicLog"\'s dynamicLogWithPP like so:
--
-- > logHook = clickablePP xmobarPP { ... } >>= dynamicLogWithPP
--
-- * Requirements:
--   * @xdotool@ on system (in path)
--   * "XMonad.Hooks.EwmhDesktops" for @xdotool@ support (see Hackage docs for setup)
--   * use of UnsafeStdinReader/UnsafeXMonadLog in xmobarrc (rather than StdinReader/XMonadLog)
--
-- Note that UnsafeStdinReader is potentially dangerous if your workspace
-- names are dynamically generated from untrusted input (like window titles).
-- You may need to add @xmobarRaw@ to 'ppRename' before applying
-- 'clickablePP' in such case.

-- | Wrap string with an xmobar action that uses @xdotool@ to switch to
-- workspace @i@.
clickableWrap :: Int -> String -> String
clickableWrap i ws = xmobarAction ("xdotool set_desktop " ++ show i) "1" ws

-- | Return a function that wraps workspace names in an xmobar action that
-- switches to that workspace.
--
-- This assumes that 'XMonad.Hooks.EwmhDesktops.ewmhDesktopsEventHook'
-- isn't configured to change the workspace order. We might need to add an
-- additional parameter if anyone needs that.
getClickable :: X (String -> WindowSpace -> String)
getClickable = getWsIndex <&> \idx s w -> maybe id clickableWrap (idx (W.tag w)) s

-- | Apply clickable wrapping to the given PP.
clickablePP :: PP -> X PP
clickablePP pp = getClickable <&> \ren -> pp{ ppRename = ppRename pp >=> ren }
