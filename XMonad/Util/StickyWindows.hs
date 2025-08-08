-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.StickyWindows
-- Description :  TODO
-- Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- TODO
--
-----------------------------------------------------------------------------
module XMonad.Util.StickyWindows (
  sticky,
  stick,
  unstick
  ) where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.ExtensibleState   as XS

data StickyState = SS
  { lastWs   :: WorkspaceId
  , stickies :: M.Map ScreenId (S.Set Window)
  }
  deriving (Show, Read)

instance ExtensionClass StickyState where
  initialValue  = SS mempty M.empty
  extensionType = PersistentExtension

modifySticky
  :: (S.Set Window -> S.Set Window) -> ScreenId -> StickyState -> StickyState
modifySticky f sid (SS ws ss) =
  SS ws $ M.alter (Just . f . fromMaybe S.empty) sid ss

modifyStickyM :: (S.Set Window -> S.Set Window) -> ScreenId -> X ()
modifyStickyM f sid = XS.modify (modifySticky f sid)

stick' :: Window -> ScreenId -> X ()
stick' = modifyStickyM . S.insert

unstick' :: Window -> ScreenId -> X ()
unstick' = modifyStickyM . S.delete

unstick :: Window -> X ()
unstick w = unstick' w =<< currentScreen

stick :: Window -> X ()
stick w = stick' w =<< currentScreen

currentScreen :: X ScreenId
currentScreen = gets $ W.screen . W.current . windowset

sticky :: XConfig l -> XConfig l
sticky xconf = xconf
  { logHook         = logHook xconf >> stickyLogHook
  , handleEventHook = handleEventHook xconf <> stickyEventHook
  }

stickyLogHook :: X ()
stickyLogHook = do
  ws <- gets $ W.current . windowset
  let sid   = W.screen ws
      wsTag = W.tag . W.workspace $ ws
  lastWS_ <- XS.gets lastWs
  unless (wsTag == lastWS_)
    $   XS.gets (M.lookup sid . stickies)
    >>= maybe mempty (moveWindows wsTag)
    >>  XS.modify (\(SS _ ws') -> SS wsTag ws')

moveWindows :: WorkspaceId -> S.Set Window -> X ()
moveWindows wsTag = traverse_ (\w -> windows $ W.focusDown . W.shiftWin wsTag w )

stickyEventHook :: Event -> X All
stickyEventHook DestroyWindowEvent { ev_window = w } =
  XS.modify (\(SS ws ss) -> SS ws (M.map (S.delete w) ss)) $> All True
stickyEventHook _ = return (All True)
