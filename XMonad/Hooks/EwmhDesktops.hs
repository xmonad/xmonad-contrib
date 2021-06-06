{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.EwmhDesktops
-- Copyright    : (c) 2007, 2008 Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes xmonad use the EWMH hints to tell panel applications about its
-- workspaces and the windows therein. It also allows the user to interact
-- with xmonad by clicking on panels and window lists.
-----------------------------------------------------------------------------
module XMonad.Hooks.EwmhDesktops (
    -- * Usage
    -- $usage
    EwmhConfig(..),
    ewmh',
    ewmh,
    ewmhDesktopsStartup',
    ewmhDesktopsStartup,
    ewmhDesktopsLogHook',
    ewmhDesktopsLogHook,
    ewmhDesktopsLogHookCustom,
    ewmhDesktopsEventHook',
    ewmhDesktopsEventHook,
    ewmhDesktopsEventHookCustom,
    ewmhFullscreen,
    fullscreenEventHook,
    fullscreenStartup,

    -- * Stuff for developers:
    addSupported,
    ) where

import Codec.Binary.UTF8.String (encode)
import Data.Bits
import qualified Data.Map.Strict as M

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import qualified XMonad.Util.ExtensibleState as E
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties (getProp32)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops
-- >
-- > main = xmonad $ … . ewmh' def{fullscreen = True} . … $ def{…}
--
-- or, if fullscreen handling is not desired, just
--
-- > main = xmonad $ … . ewmh . … $ def{…}
--
-- You may also be interested in 'docks' from "XMonad.Hooks.ManageDocks".
--
-- TODO: mention "XMonad.Hooks.UrgencyHook"
-- TODO: mention "XMonad.Hooks.Focus"

-- | TODO
data EwmhConfig = EwmhConfig
    { workspaceListSort :: X ([WindowSpace] -> [WindowSpace])
    , workspaceRename :: X (WindowSpace -> WindowSpace)
    , activateHook :: ManageHook
    , fullscreen :: Bool
    }

instance Default EwmhConfig where
    def = EwmhConfig
        { workspaceListSort = pure id
        , workspaceRename = pure id
        , activateHook = doFocus
        , fullscreen = False
        }

-- | 'ewmh'' with default 'EwmhConfig'.
ewmh :: XConfig a -> XConfig a
ewmh = ewmh' def

-- | Add EWMH functionality to the given config.  See above for an example.
ewmh' :: EwmhConfig -> XConfig a -> XConfig a
ewmh' ewmhConfig xConfig =
    xConfig{ startupHook     = ewmhDesktopsStartup'   ewmhConfig <+> startupHook xConfig
           , handleEventHook = ewmhDesktopsEventHook' ewmhConfig <+> handleEventHook xConfig
           , logHook         = ewmhDesktopsLogHook'   ewmhConfig <+> logHook xConfig
           }

-- | 'ewmhDesktopsStartup'' with default 'EwmhConfig'.
ewmhDesktopsStartup :: X ()
ewmhDesktopsStartup = ewmhDesktopsStartup' def

-- |
-- Initializes EwmhDesktops and advertises EWMH support to the X
-- server
ewmhDesktopsStartup' :: EwmhConfig -> X ()
ewmhDesktopsStartup' = setSupported

-- |
-- Cached desktop names (e.g. @_NET_NUMBER_OF_DESKTOPS@ and
-- @_NET_DESKTOP_NAMES@).
newtype DesktopNames = DesktopNames [String]
                     deriving (Eq)

instance ExtensionClass DesktopNames where
    initialValue = DesktopNames []

-- |
-- Cached client list (e.g. @_NET_CLIENT_LIST@).
newtype ClientList = ClientList [Window]
                   deriving (Eq)

instance ExtensionClass ClientList where
    initialValue = ClientList [none]

-- |
-- Cached current desktop (e.g. @_NET_CURRENT_DESKTOP@).
newtype CurrentDesktop = CurrentDesktop Int
                       deriving (Eq)

instance ExtensionClass CurrentDesktop where
    initialValue = CurrentDesktop (-1)

-- |
-- Cached window-desktop assignments (e.g. @_NET_CLIENT_LIST_STACKING@).
newtype WindowDesktops = WindowDesktops (M.Map Window Int)
                       deriving (Eq)

instance ExtensionClass WindowDesktops where
    initialValue = WindowDesktops (M.singleton none (-1))

-- |
-- The value of @_NET_ACTIVE_WINDOW@, cached to avoid unnecessary property
-- updates.
newtype ActiveWindow = ActiveWindow Window
                     deriving (Eq)

instance ExtensionClass ActiveWindow where
    initialValue = ActiveWindow (complement none)

-- | Compare the given value against the value in the extensible state. Run the
-- action if it has changed.
whenChanged :: (Eq a, ExtensionClass a) => a -> X () -> X ()
whenChanged v action = do
    v0 <- E.get
    unless (v == v0) $ do
        action
        E.put v

-- | 'ewmhDesktopsLogHook'' with default 'EwmhConfig'.
ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = ewmhDesktopsLogHook' def

-- |
-- Generalized version of ewmhDesktopsLogHook that allows an arbitrary
-- user-specified function to transform the workspace list (post-sorting)
{-# DEPRECATED ewmhDesktopsLogHookCustom "Use ewmhDesktopsLogHook' instead" #-}
ewmhDesktopsLogHookCustom :: ([WindowSpace] -> [WindowSpace]) -> X ()
ewmhDesktopsLogHookCustom f = ewmhDesktopsLogHook' def{ workspaceListSort = pure f }

-- |
-- Notifies pagers and window lists, such as those in the gnome-panel
-- of the current state of workspaces and windows.
ewmhDesktopsLogHook' :: EwmhConfig -> X ()
ewmhDesktopsLogHook' EwmhConfig{workspaceListSort, workspaceRename} = withWindowSet $ \s -> do
    sort' <- getSortByIndex
    workspaceListSort' <- workspaceListSort
    workspaceRename' <- workspaceRename
    let wsTransform = map workspaceRename' . workspaceListSort'
    let ws = wsTransform $ sort' $ W.workspaces s

    -- Set number of workspaces and names thereof
    let desktopNames = map W.tag ws
    whenChanged (DesktopNames desktopNames) $ do
        setNumberOfDesktops (length desktopNames)
        setDesktopNames desktopNames

    -- Set client list; all windows, with focused windows last
    let clientList = nub . concatMap (maybe [] (\(W.Stack x l r) -> reverse l ++ r ++ [x]) . W.stack) $ ws
    whenChanged (ClientList clientList) $ setClientList clientList

    -- Remap the current workspace to handle any renames that wsTransform might be doing.
    let maybeCurrent' = W.tag <$> listToMaybe (wsTransform [W.workspace $ W.current s])
        current = join (flip elemIndex (map W.tag ws) <$> maybeCurrent')
    whenChanged (CurrentDesktop $ fromMaybe 0 current) $
        mapM_ setCurrentDesktop current

    -- Set window-desktop mapping
    let windowDesktops =
          let f wsId workspace = M.fromList [ (winId, wsId) | winId <- W.integrate' $ W.stack workspace ]
          in M.unions $ zipWith f [0..] ws
    whenChanged (WindowDesktops windowDesktops) $
        mapM_ (uncurry setWindowDesktop) (M.toList windowDesktops)

    -- Set active window
    let activeWindow' = fromMaybe none (W.peek s)
    whenChanged (ActiveWindow activeWindow') $ setActiveWindow activeWindow'

-- | 'ewmhDesktopsEventHook'' with default 'EwmhConfig'.
ewmhDesktopsEventHook :: Event -> X All
ewmhDesktopsEventHook = ewmhDesktopsEventHook' def

-- |
-- Generalized version of ewmhDesktopsEventHook that allows an arbitrary
-- user-specified function to transform the workspace list (post-sorting)
{-# DEPRECATED ewmhDesktopsEventHookCustom "Use ewmhDesktopsEventHook' instead" #-}
ewmhDesktopsEventHookCustom :: ([WindowSpace] -> [WindowSpace]) -> Event -> X All
ewmhDesktopsEventHookCustom f = ewmhDesktopsEventHook' def{ workspaceListSort = pure f }

-- |
-- Intercepts messages from pagers and similar applications and reacts on them.
-- Currently supports:
--
--  * _NET_CURRENT_DESKTOP (switching desktops)
--
--  * _NET_WM_DESKTOP (move windows to other desktops)
--
--  * _NET_ACTIVE_WINDOW (activate another window, changing workspace if needed)
--
--  * _NET_CLOSE_WINDOW (close window)
--
--  * _NET_WM_STATE with _NET_WM_STATE_FULLSCREEN property (other properties
--    handled by other modules like "XMonad.Hooks.ManageHelpers",
--    "XMonad.Actions.Minimize", etc.)
ewmhDesktopsEventHook' :: EwmhConfig -> Event -> X All
ewmhDesktopsEventHook' EwmhConfig{ workspaceListSort, activateHook, fullscreen }
        e@ClientMessageEvent{ev_window = w, ev_message_type = mt, ev_data = d} =
    withWindowSet $ \s -> do
        sort' <- getSortByIndex
        workspaceListSort' <- workspaceListSort
        let ws = workspaceListSort' $ sort' $ W.workspaces s

        a_cd <- getAtom "_NET_CURRENT_DESKTOP"
        a_d <- getAtom "_NET_WM_DESKTOP"
        a_aw <- getAtom "_NET_ACTIVE_WINDOW"
        a_cw <- getAtom "_NET_CLOSE_WINDOW"
        a_ws <- getAtom "_NET_WM_STATE"

        if  | mt == a_cd, n : _ <- d, Just ww <- ws !? fi n ->
                if W.currentTag s == W.tag ww then mempty else windows $ W.view (W.tag ww)
            | mt == a_cd ->
                trace $ "Bad _NET_CURRENT_DESKTOP with data=" ++ show d
            | mt == a_d, n : _ <- d, Just ww <- ws !? fi n ->
                if W.findTag w s == Just (W.tag ww) then mempty else windows $ W.shiftWin (W.tag ww) w
            | mt == a_d ->
                trace $ "Bad _NET_WM_DESKTOP with data=" ++ show d
            | mt == a_aw, W.peek s /= Just w -> case d of
                -- when the request comes from a pager, honor it unconditionally
                -- https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html#sourceindication
                2 : _ -> windows $ W.focusWindow w
                _ -> windows . appEndo =<< runQuery activateHook w
            | mt == a_cw ->
                killWindow w
            | mt == a_ws && fullscreen ->
                void $ fullscreenEventHook' e
            | otherwise -> do
                -- The Message is unknown to us, but that is ok, not all are meant
                -- to be handled by the window manager
                return ()

        return (All True)
ewmhDesktopsEventHook' _ _ = return (All True)

-- | Add EWMH fullscreen functionality to the given config.
--
-- This must be applied after 'ewmh', like so:
--
-- > main = xmonad $ ewmhFullscreen $ ewmh def
--
-- NOT:
--
-- > main = xmonad $ ewmh $ ewmhFullscreen def
{-# DEPRECATED ewmhFullscreen "Use ewmh' def{fullscreen = True} instead" #-}
ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen c = c { startupHook     = startupHook c <+> fullscreenStartup
                     , handleEventHook = handleEventHook c <+> fullscreenEventHook }

-- | Advertises EWMH fullscreen support to the X server.
{-# DEPRECATED fullscreenStartup "Use ewmh' def{fullscreen = True} instead" #-}
fullscreenStartup :: X ()
fullscreenStartup = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

-- |
-- An event hook to handle applications that wish to fullscreen using the
-- _NET_WM_STATE protocol. This includes users of the gtk_window_fullscreen()
-- function, such as Totem, Evince and OpenOffice.org.
{-# DEPRECATED fullscreenEventHook "Use ewmh' def{fullscreen = True} instead" #-}
fullscreenEventHook :: Event -> X All
fullscreenEventHook = fullscreenEventHook'

fullscreenEventHook' :: Event -> X All
fullscreenEventHook' (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  managed <- isClient win
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> getProp32 wmstate win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      chWstate f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)

  when (managed && typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      windows $ W.sink win

  return $ All True

fullscreenEventHook' _ = return $ All True

setNumberOfDesktops :: (Integral a) => a -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    r <- asks theRoot
    io $ changeProperty32 dpy r a cARDINAL propModeReplace [fromIntegral n]

setCurrentDesktop :: (Integral a) => a -> X ()
setCurrentDesktop i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_CURRENT_DESKTOP"
    r <- asks theRoot
    io $ changeProperty32 dpy r a cARDINAL propModeReplace [fromIntegral i]

setDesktopNames :: [String] -> X ()
setDesktopNames names = withDisplay $ \dpy -> do
    -- Names thereof
    r <- asks theRoot
    a <- getAtom "_NET_DESKTOP_NAMES"
    c <- getAtom "UTF8_STRING"
    let names' = map fromIntegral $ concatMap ((++[0]) . encode) names
    io $ changeProperty8 dpy r a c propModeReplace names'

setClientList :: [Window] -> X ()
setClientList wins = withDisplay $ \dpy -> do
    -- (What order do we really need? Something about age and stacking)
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST"
    io $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fromIntegral wins)
    a' <- getAtom "_NET_CLIENT_LIST_STACKING"
    io $ changeProperty32 dpy r a' wINDOW propModeReplace (fmap fromIntegral wins)

setWindowDesktop :: (Integral a) => Window -> a -> X ()
setWindowDesktop win i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_DESKTOP"
    io $ changeProperty32 dpy win a cARDINAL propModeReplace [fromIntegral i]

setSupported :: EwmhConfig -> X ()
setSupported EwmhConfig{ fullscreen } = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    supp <- mapM getAtom $
        [ "_NET_WM_STATE_HIDDEN"
        , "_NET_NUMBER_OF_DESKTOPS"
        , "_NET_CLIENT_LIST"
        , "_NET_CLIENT_LIST_STACKING"
        , "_NET_CURRENT_DESKTOP"
        , "_NET_DESKTOP_NAMES"
        , "_NET_ACTIVE_WINDOW"
        , "_NET_WM_DESKTOP"
        , "_NET_WM_STRUT"
        ] ++
        if fullscreen then ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"] else []
    io $ changeProperty32 dpy r a aTOM propModeReplace (fmap fromIntegral (nub supp))

    setWMName "xmonad"

-- TODO: use in SetWMName, UrgencyHook
addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
        supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
        changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)

setActiveWindow :: Window -> X ()
setActiveWindow w = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_ACTIVE_WINDOW"
    io $ changeProperty32 dpy r a wINDOW propModeReplace [fromIntegral w]
