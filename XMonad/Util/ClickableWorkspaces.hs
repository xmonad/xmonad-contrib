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
  clickablePP
  ) where

import XMonad
import XMonad.Util.WorkspaceCompare (getWsIndex)
import XMonad.Hooks.DynamicLog (xmobarAction, xmobarRaw, PP(..))

-- $usage
-- However you have set up your PP, apply @clickablePP@ to it, and bind the result
-- to "XMonad.Hooks.DynamicLog"\'s dynamicLogWithPP like so:
-- 
-- > logHook = clickablePP xmobarPP { ... } >>= dynamicLogWithPP
--
-- * Requirements:
--   * wmctrl on system (in path)
--   * "XMonad.Hooks.EwmhDesktops" for wmctrl support (see Hackage docs for setup)
--   * use of UnsafeStdinReader in xmobarrc (rather than StdinReader)


clickableWrap :: Int -> String -> String
clickableWrap i ws = xmobarAction ("wmctrl -s " ++ show i) "1" $ xmobarRaw ws

-- Use index of workspace in users config to target workspace with wmctrl switch.
getClickable :: X (WorkspaceId -> String)
getClickable = do
  wsIndex <- getWsIndex
  return $ \ws -> case wsIndex ws of
                    Just idx -> clickableWrap idx ws
                    Nothing -> ws

-- | Apply clickable wrapping to all workspace fields in given PP.
clickablePP :: PP -> X PP
clickablePP pp = do
  clickable <- getClickable
  return $
    pp { ppCurrent         = ppCurrent pp . clickable
       , ppVisible         = ppVisible pp . clickable
       , ppHidden          = ppHidden pp . clickable
       , ppHiddenNoWindows = ppHiddenNoWindows pp . clickable
       , ppUrgent          = ppUrgent pp . clickable
       }
