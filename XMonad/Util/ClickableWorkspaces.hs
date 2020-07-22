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
import XMonad.Hooks.DynamicLog (PP(..))

-- $usage
-- However you have set up your PP, apply @clickablePP@ to it, and bind the result
-- to "XMonad.Hooks.DynamicLog"\'s dynamicLogWithPP like so:
-- 
-- > myPP xmprocs = xmobarPP { ... }
-- > myLogHook xmprocs = (clickablePP . myPP) xmprocs >>= dynamicLogWithPP
--
-- * Requirements:
--   * wmctrl on system (in path)
--   * "XMonad.Hooks.EwmhDesktops" for wmctrl support (see Hackage docs for setup)
--   * use of UnsafeStdinReader in xmobarrc (rather than StdinReader)


-- In case workspace tags include any '<', escape them
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

clickableWrap :: Integer -> String -> String
clickableWrap num ws =
  "<action=wmctrl -s " ++ show num ++ ">" ++ xmobarEscape ws ++ "</action>"

-- Use index of workspace in users config to target workspace with wmctrl switch.
getClickable :: X (WorkspaceId -> String)
getClickable = do
  wsIndex <- getWsIndex
  return $ \ws -> case wsIndex ws of
                    Just idx -> clickableWrap (toInteger idx) ws
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
