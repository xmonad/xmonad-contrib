-- |
-- Module      :  XMonad.Util.StickyWindows
-- Description :  Make windows sticky to a screen across workspace changes.
-- Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides functionality to make windows \"sticky\" to a particular
-- screen. When a window is marked as sticky on a screen, it will automatically
-- follow that screen across workspace changes, staying visible even when you
-- switch to a different workspace.
--
-- This is particularly useful for windows you want to keep visible at all times
-- on a specific monitor, such as Picture-in-Picture videos, music players,
-- communication apps, or reference documentation.
module XMonad.Util.StickyWindows (
    -- * Usage
    -- $usage
    sticky,
    stick,
    unstick,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Util.StickyWindows
--
-- To enable sticky windows, wrap your config with 'sticky':
--
-- > main = xmonad $ … . sticky . … $ def { ... }
--
-- This adds the necessary hooks to manage sticky windows. Next, add keybindings
-- to stick and unstick windows:
--
-- > , ((modMask, xK_s), withFocused stick)
-- > , ((modMask .|. shiftMask, xK_s), withFocused unstick)
--
-- Now you can:
--
--   1. Focus a window and press @Mod-s@ to make it sticky to the current screen
--   2. Switch workspaces on that screen, and the sticky window will follow
--   3. Press @Mod-Shift-s@ to unstick the window
--
-- Note that windows are sticky to a /specific screen/, not to all screens. If you
-- have multiple monitors, a window marked sticky on screen 0 will only follow
-- workspace changes on screen 0, not on other screens.
--
-- The sticky state persists across XMonad restarts.

data StickyState = SS
    { lastWs :: !(M.Map ScreenId WorkspaceId)
    , stickies :: !(M.Map ScreenId (S.Set Window))
    }
    deriving (Show, Read)

instance ExtensionClass StickyState where
    initialValue = SS mempty mempty
    extensionType = PersistentExtension

modifySticky ::
    (S.Set Window -> S.Set Window) -> ScreenId -> StickyState -> StickyState
modifySticky f sid (SS ws ss) =
    SS ws $ M.alter (Just . f . fromMaybe S.empty) sid ss

modifyStickyM :: (S.Set Window -> S.Set Window) -> ScreenId -> X ()
modifyStickyM f sid = XS.modify (modifySticky f sid)

stick' :: Window -> ScreenId -> X ()
stick' = modifyStickyM . S.insert

unstick' :: Window -> ScreenId -> X ()
unstick' = modifyStickyM . S.delete

-- | Remove the sticky status from the given window on the current screen.
-- The window will no longer automatically follow workspace changes.
--
-- Typically used with 'withFocused':
--
-- > , ((modMask .|. shiftMask, xK_s), withFocused unstick)
unstick :: Window -> X ()
unstick w = unstick' w =<< currentScreen

-- | Mark the given window as sticky to the current screen. The window will
-- automatically follow this screen across workspace changes until explicitly
-- unstuck with 'unstick' or until the window is destroyed.
--
-- Typically used with 'withFocused':
--
-- > , ((modMask, xK_s), withFocused stick)
stick :: Window -> X ()
stick w = stick' w =<< currentScreen

currentScreen :: X ScreenId
currentScreen = gets $ W.screen . W.current . windowset

-- | Incorporates sticky window functionality into an 'XConfig'. This adds
-- the necessary log hook and event hook to:
--
--   * Automatically move sticky windows when workspaces change on their screen
--   * Clean up sticky state when windows are destroyed
--
-- Example usage:
--
-- > main = xmonad $ … . sticky .  … $ def { ... }
sticky :: XConfig l -> XConfig l
sticky xconf =
    xconf
        { logHook = logHook xconf >> stickyLogHook
        , handleEventHook = handleEventHook xconf <> stickyEventHook
        }

stickyLogHook :: X ()
stickyLogHook = do
    lastWS_ <- XS.gets lastWs
    screens <- withWindowSet $ return . map (\s -> (W.screen s, W.tag . W.workspace $ s)) . W.screens
    for_ screens $ \(sid, wsTag) -> do
        unless (M.lookup sid lastWS_ == Just wsTag) $
            -- We need to update the last workspace before moving windows to avoid
            -- getting stuck in a loop: This is a log hook, and calling moveWindows
            -- (which in turn calls 'windows') would trigger another log hook.
            XS.modify (\(SS ws ss) -> SS (M.insert sid wsTag ws) ss)
                >> XS.gets (M.lookup sid . stickies)
                >>= maybe mempty (moveWindows wsTag)

moveWindows :: WorkspaceId -> S.Set Window -> X ()
moveWindows wsTag = traverse_ (\w -> windows $ W.focusDown . W.shiftWin wsTag w)

stickyEventHook :: Event -> X All
stickyEventHook DestroyWindowEvent{ev_window = w} =
    XS.modify (\(SS ws ss) -> SS ws (M.map (S.delete w) ss)) $> All True
stickyEventHook _ = return (All True)
