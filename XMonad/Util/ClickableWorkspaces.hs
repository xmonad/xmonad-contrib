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
--   * xdotool on system (in path)
--   * use of UnsafeStdinReader in xmobarrc (rather than StdinReader)
--   * Workspace switch bindings are expected to be "super + i",
--     where i is the workspace index (beginning with 1), as in default bindings.


-- In case workspace tags include any '<', escape them
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

clickableWrap :: Integer -> String -> String
clickableWrap num ws =
  "<action=xdotool key super+" ++ show num ++ ">" ++ xmobarEscape ws ++ "</action>"

-- Use index of workspace in users config to assign workspace switch binding.
getClickable :: X (WorkspaceId -> String)
getClickable = do
  wsIndex <- getWsIndex
  return $ \ws -> case wsIndex ws of
                    Just idx -> clickableWrap (indexToKey idx) ws
                    Nothing -> ws
  where indexToKey = (+ 1) . toInteger

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
