{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

-- |
-- Module      :  XMonad.Hooks.EWMH.Desktops
-- Description :  Extended Window Manager Hints (EWMH) support for workspaces (virtual desktops).
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- Makes xmonad use the EWMH hints to tell panel applications about its
-- workspaces and the windows therein. It also allows the user to interact
-- with xmonad by clicking on panels and window lists.
--

module XMonad.Hooks.EWMH.Desktops (
    -- * Usage
    -- $usage
    ewmhDesktops,
    setEwmhWorkspaceListTransform,
    addEwmhWorkspaceListTransform,
    ) where

import Codec.Binary.UTF8.String (encode)
import Data.Bits (complement)
import XMonad
import XMonad.Prelude
import XMonad.Util.EWMH
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleConf as XC
import qualified XMonad.Util.ExtensibleState as XS

-- ---------------------------------------------------------------------
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > main = xmonad $ … . ewmhDesktops . … $ def{…}

newtype EwmhDesktopsConfig =
    EwmhDesktopsConfig
        { workspaceListTransform :: [WindowSpace] -> [WindowSpace]
        }

instance Default EwmhDesktopsConfig where
    def = EwmhDesktopsConfig
        { workspaceListTransform = id
        }

data EwmhDesktops = EwmhDesktops

-- | Add EWMH support for workspaces (virtual desktops) to 'XConfig'.
ewmhDesktops :: XConfig l -> XConfig l
ewmhDesktops = ewmhSupported hints . XC.onceIni EwmhDesktops hooks
  where
    hints = [ "_NET_DESKTOP_NAMES"
            , "_NET_NUMBER_OF_DESKTOPS"
            , "_NET_CLIENT_LIST"
            , "_NET_CLIENT_LIST_STACKING"
            , "_NET_CURRENT_DESKTOP"
            , "_NET_WM_DESKTOP"
            , "_NET_ACTIVE_WINDOW"
            , "_NET_CLOSE_WINDOW"
            ]
    hooks c = c{ handleEventHook = handleEventHook c <> ewmhDesktopsEventHook
               , logHook = logHook c <> ewmhDesktopsLogHook }

-- | Set an arbitrary user-specified function to transform the workspace list
-- (post-sorting). This can be used to e.g. filter out scratchpad workspaces.
setEwmhWorkspaceListTransform :: ([WindowSpace] -> [WindowSpace]) -> XConfig l -> XConfig l
setEwmhWorkspaceListTransform f = XC.modifyDef $ \c -> c{ workspaceListTransform = f }

-- | Like 'setEwmhWorkspaceListTransform', but compose (after) with the
-- existing instead of replacing it.
addEwmhWorkspaceListTransform :: ([WindowSpace] -> [WindowSpace]) -> XConfig l -> XConfig l
addEwmhWorkspaceListTransform f = XC.modifyDef $ \c ->
    c{ workspaceListTransform = f . workspaceListTransform c }

ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = XC.withDef $ \EwmhDesktopsConfig{workspaceListTransform} -> do
    withWindowSet $ \s -> do
        sort' <- getSortByIndex
        let ws = workspaceListTransform $ sort' $ W.workspaces s

        -- Set number of workspaces and names thereof
        let desktopNames = map W.tag ws
        whenModified (NetDesktopNames desktopNames) $ do
            setNumberOfDesktops (length desktopNames)
            setDesktopNames desktopNames

        -- Set client list which should be sorted by window age. We just
        -- guess that StackSet contains windows list in this order which
        -- isn't true but at least gives consistency with windows cycling
        let clientList = nub . concatMap (W.integrate' . W.stack) $ ws
        whenModified (NetClientList clientList) $ do
            setClientList clientList

        -- Set stacking client list which should have bottom-to-top
        -- stacking order, i.e. focused window should be last
        let clientListStacking = nub . concatMap (maybe [] (\(W.Stack x l r) -> reverse l ++ r ++ [x]) . W.stack) $ ws
        whenModified (NetClientListStacking clientListStacking) $ do
            setClientListStacking clientListStacking

        -- Set current desktop (remap the current workspace to handle any
        -- renames that workspaceListTransform might be doing).
        let maybeCurrent' = W.tag <$> listToMaybe (workspaceListTransform [W.workspace $ W.current s])
            current = flip elemIndex (map W.tag ws) =<< maybeCurrent'
        whenModified (NetCurrentDesktop $ fromMaybe 0 current) $
            mapM_ setCurrentDesktop current

        -- Set window-desktop mapping
        let windowDesktops =
              let f wsId workspace = M.fromList [ (winId, wsId) | winId <- W.integrate' $ W.stack workspace ]
              in M.unions $ zipWith f [0..] ws
        whenModified (NetWmDesktop windowDesktops) $
            mapM_ (uncurry setWindowDesktop) (M.toList windowDesktops)

        -- Set active window
        let activeWindow = fromMaybe none (W.peek s)
        whenModified (NetActiveWindow activeWindow) $ do
            setActiveWindow activeWindow

ewmhDesktopsEventHook :: Event -> X All
ewmhDesktopsEventHook ClientMessageEvent{ev_window = w, ev_message_type = mt, ev_data = d} =
    XC.withDef $ \EwmhDesktopsConfig{workspaceListTransform} ->
    withWindowSet $ \s -> do
        sort' <- getSortByIndex
        let ws = workspaceListTransform $ sort' $ W.workspaces s

        a_cd <- getAtom "_NET_CURRENT_DESKTOP"
        a_d <- getAtom "_NET_WM_DESKTOP"
        a_aw <- getAtom "_NET_ACTIVE_WINDOW"
        a_cw <- getAtom "_NET_CLOSE_WINDOW"

        if  | mt == a_cd, n : _ <- d, Just ww <- ws !? fi n ->
                if W.currentTag s == W.tag ww then mempty else windows $ W.view (W.tag ww)
            | mt == a_cd ->
                trace $ "Bad _NET_CURRENT_DESKTOP with data=" ++ show d
            | mt == a_d, n : _ <- d, Just ww <- ws !? fi n ->
                if W.findTag w s == Just (W.tag ww) then mempty else windows $ W.shiftWin (W.tag ww) w
            | mt == a_d ->
                trace $ "Bad _NET_WM_DESKTOP with data=" ++ show d
            | mt == a_aw, 2 : _ <- d ->
                -- when the request comes from a pager, honor it unconditionally
                -- https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html#sourceindication
                windows $ W.focusWindow w
            | mt == a_aw, W.peek s /= Just w -> do
                -- TODO: activateHook
                windows $ W.focusWindow w
            | mt == a_cw ->
                killWindow w
            | otherwise ->
                -- The Message is unknown to us, but that is ok, not all are meant
                -- to be handled by the window manager
                mempty

        mempty
ewmhDesktopsEventHook _ = mempty

-- | Cached @_NET_DESKTOP_NAMES@, @_NET_NUMBER_OF_DESKTOPS@
newtype NetDesktopNames = NetDesktopNames [String] deriving Eq
instance ExtensionClass NetDesktopNames where initialValue = NetDesktopNames []

-- | Cached @_NET_CLIENT_LIST@
newtype NetClientList = NetClientList [Window] deriving Eq
instance ExtensionClass NetClientList where initialValue = NetClientList [none]

-- | Cached @_NET_CLIENT_LIST_STACKING@
newtype NetClientListStacking = NetClientListStacking [Window] deriving Eq
instance ExtensionClass NetClientListStacking where initialValue = NetClientListStacking [none]

-- | Cached @_NET_CURRENT_DESKTOP@
newtype NetCurrentDesktop = NetCurrentDesktop Int deriving Eq
instance ExtensionClass NetCurrentDesktop where initialValue = NetCurrentDesktop (complement 0)

-- | Cached @_NET_WM_DESKTOP@
newtype NetWmDesktop = NetWmDesktop (M.Map Window Int) deriving Eq
instance ExtensionClass NetWmDesktop where initialValue = NetWmDesktop (M.singleton none (complement 0))

-- | Cached @_NET_ACTIVE_WINDOW@
newtype NetActiveWindow = NetActiveWindow Window deriving Eq
instance ExtensionClass NetActiveWindow where initialValue = NetActiveWindow (complement none)

-- | Update value in extensible state, run action if it changed.
whenModified :: (Eq a, ExtensionClass a) => a -> X () -> X ()
whenModified = whenX . XS.modified . const

setNumberOfDesktops :: Int -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    r <- asks theRoot
    io $ changeProperty32 dpy r a cARDINAL propModeReplace [fi n]

setDesktopNames :: [String] -> X ()
setDesktopNames names = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_DESKTOP_NAMES"
    c <- getAtom "UTF8_STRING"
    let enc = map fi . concatMap ((++[0]) . encode)
    io $ changeProperty8 dpy r a c propModeReplace $ enc names

setClientList :: [Window] -> X ()
setClientList wins = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST"
    io $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fi wins)

setClientListStacking :: [Window] -> X ()
setClientListStacking wins = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST_STACKING"
    io $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fi wins)

setCurrentDesktop :: Int -> X ()
setCurrentDesktop i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_CURRENT_DESKTOP"
    r <- asks theRoot
    io $ changeProperty32 dpy r a cARDINAL propModeReplace [fi i]

setWindowDesktop :: Window -> Int -> X ()
setWindowDesktop win i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_DESKTOP"
    io $ changeProperty32 dpy win a cARDINAL propModeReplace [fi i]

setActiveWindow :: Window -> X ()
setActiveWindow w = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_ACTIVE_WINDOW"
    io $ changeProperty32 dpy r a wINDOW propModeReplace [fi w]
