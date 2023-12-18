{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.EwmhDesktops
-- Description  : Make xmonad use the extended window manager hints (EWMH).
-- Copyright    : (c) 2007, 2008 Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes xmonad use the
-- <https://specifications.freedesktop.org/wm-spec/latest/ EWMH>
-- hints to tell panel applications about its workspaces and the windows
-- therein. It also allows the user to interact with xmonad by clicking on
-- panels and window lists.
-----------------------------------------------------------------------------
module XMonad.Hooks.EwmhDesktops (
    -- * Usage
    -- $usage
    ewmh,
    ewmhFullscreen,
    ewmhDesktopsManageHook,
    ewmhDesktopsMaybeManageHook,

    -- * Customization
    -- $customization

    -- ** Sorting/filtering of workspaces
    -- $customSort
    addEwmhWorkspaceSort, setEwmhWorkspaceSort,

    -- ** Renaming of workspaces
    -- $customRename
    addEwmhWorkspaceRename, setEwmhWorkspaceRename,

    -- ** Window activation
    -- $customActivate
    setEwmhActivateHook,

    -- ** Fullscreen
    -- $customFullscreen
    setEwmhFullscreenHooks,

    -- ** @_NET_DESKTOP_VIEWPORT@
    -- $customManageDesktopViewport
    disableEwmhManageDesktopViewport,

    -- * Standalone hooks (deprecated)
    ewmhDesktopsStartup,
    ewmhDesktopsLogHook,
    ewmhDesktopsLogHookCustom,
    ewmhDesktopsEventHook,
    ewmhDesktopsEventHookCustom,
    fullscreenEventHook,
    fullscreenStartup,
    ) where

import Codec.Binary.UTF8.String (encode)
import Data.Bits
import qualified Data.Map.Strict as M

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties (getProp32)
import qualified XMonad.Util.ExtensibleConf as XC
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops
-- >
-- > main = xmonad $ … . ewmhFullscreen . ewmh . … $ def{…}
--
-- or, if fullscreen handling is not desired, just
--
-- > main = xmonad $ … . ewmh . … $ def{…}
--
-- You may also be interested in 'XMonad.Hooks.ManageDocks.docks' and
-- 'XMonad.Hooks.UrgencyHook.withUrgencyHook', which provide support for other
-- parts of the
-- <https://specifications.freedesktop.org/wm-spec/latest/ EWMH specification>.

-- | Add EWMH support for workspaces (virtual desktops) to the given
-- 'XConfig'.  See above for an example.
ewmh :: XConfig a -> XConfig a
ewmh c = c { startupHook     = ewmhDesktopsStartup <> startupHook c
           , handleEventHook = ewmhDesktopsEventHook <> handleEventHook c
           , logHook         = ewmhDesktopsLogHook <> logHook c }


-- $customization
-- It's possible to customize the behaviour of 'ewmh' in several ways:

-- | Customizable configuration for EwmhDesktops
data EwmhDesktopsConfig =
    EwmhDesktopsConfig
        { workspaceSort :: X WorkspaceSort
            -- ^ configurable workspace sorting/filtering
        , workspaceRename :: X (String -> WindowSpace -> String)
            -- ^ configurable workspace rename (see 'XMonad.Hooks.StatusBar.PP.ppRename')
        , activateHook :: ManageHook
            -- ^ configurable handling of window activation requests
        , fullscreenHooks :: (ManageHook, ManageHook)
            -- ^ configurable handling of fullscreen state requests
        , manageDesktopViewport :: Bool
            -- ^ manage @_NET_DESKTOP_VIEWPORT@?
        }

instance Default EwmhDesktopsConfig where
    def = EwmhDesktopsConfig
        { workspaceSort = getSortByIndex
        , workspaceRename = pure pure
        , activateHook = doFocus
        , fullscreenHooks = (doFullFloat, doSink)
        , manageDesktopViewport = True
        }


-- $customSort
-- The list of workspaces exposed to EWMH pagers (like
-- <https://github.com/taffybar/taffybar taffybar> and
-- <https://github.com/polybar/polybar polybar>) and clients (such as
-- <http://tomas.styblo.name/wmctrl/ wmctrl> and
-- <https://github.com/jordansissel/xdotool/ xdotool>) may be sorted and/or
-- filtered via a user-defined function.
--
-- To show visible workspaces first, one may switch to a Xinerama-aware
-- sorting function:
--
-- > import XMonad.Util.WorkspaceCompare
-- >
-- > mySort = getSortByXineramaRule
-- > main = xmonad $ … . setEwmhWorkspaceSort mySort . ewmh . … $ def{…}
--
-- Another useful example is not exposing the hidden scratchpad workspace:
--
-- > import XMonad.Util.NamedScratchpad
-- > import XMonad.Util.WorkspaceCompare
-- >
-- > myFilter = filterOutWs [scratchpadWorkspaceTag]
-- > main = xmonad $ … . addEwmhWorkspaceSort (pure myFilter) . ewmh . … $ def{…}

-- | Add (compose after) an arbitrary user-specified function to sort/filter
-- the workspace list. The default/initial function is 'getSortByIndex'. This
-- can be used to e.g. filter out scratchpad workspaces. Workspaces /must not/
-- be renamed here.
addEwmhWorkspaceSort :: X WorkspaceSort -> XConfig l -> XConfig l
addEwmhWorkspaceSort f = XC.modifyDef $ \c -> c{ workspaceSort = liftA2 (.) f (workspaceSort c) }

-- | Like 'addEwmhWorkspaceSort', but replace it instead of adding/composing.
setEwmhWorkspaceSort :: X WorkspaceSort -> XConfig l -> XConfig l
setEwmhWorkspaceSort f = XC.modifyDef $ \c -> c{ workspaceSort = f }


-- $customRename
-- The workspace names exposed to EWMH pagers and other clients (e.g.
-- <https://arbtt.nomeata.de/ arbtt>) may be altered using a similar
-- interface to 'XMonad.Hooks.StatusBar.PP.ppRename'. To configure workspace
-- renaming, use 'addEwmhWorkspaceRename'.
--
-- As an example, to expose workspaces uppercased:
--
-- > import Data.Char
-- >
-- > myRename :: String -> WindowSpace -> String
-- > myRename s _w = map toUpper s
-- >
-- > main = xmonad $ … . addEwmhWorkspaceRename (pure myRename) . ewmh . … $ def{…}
--
-- Some modules like "XMonad.Actions.WorkspaceNames" provide ready-made
-- integrations:
--
-- > import XMonad.Actions.WorkspaceNames
-- >
-- > main = xmonad $ … . workspaceNamesEwmh . ewmh . … $ def{…}
--
-- The above ensures workspace names are exposed through EWMH.

-- | Add (compose after) an arbitrary user-specified function to rename each
-- workspace. This works just like 'XMonad.Hooks.StatusBar.PP.ppRename': the
-- @WindowSpace -> …@ acts as a Reader monad. Useful with
-- "XMonad.Actions.WorkspaceNames", "XMonad.Layout.IndependentScreens",
-- "XMonad.Hooks.DynamicIcons".
addEwmhWorkspaceRename :: X (String -> WindowSpace -> String) -> XConfig l -> XConfig l
addEwmhWorkspaceRename f = XC.modifyDef $ \c -> c{ workspaceRename = liftA2 (<=<) f (workspaceRename c) }

-- | Like 'addEwmhWorkspaceRename', but replace it instead of adding/composing.
setEwmhWorkspaceRename :: X (String -> WindowSpace -> String) -> XConfig l -> XConfig l
setEwmhWorkspaceRename f = XC.modifyDef $ \c -> c{ workspaceRename = f }


-- $customActivate
-- When a client sends a @_NET_ACTIVE_WINDOW@ request to activate a window, by
-- default that window is activated by invoking the 'doFocus' 'ManageHook'.
-- <https://specifications.freedesktop.org/wm-spec/1.5/ar01s03.html#idm45623294083744 The EWMH specification suggests>
-- that a window manager may instead just mark the window as urgent, and this
-- can be achieved using the following:
--
-- > import XMonad.Hooks.UrgencyHook
-- >
-- > main = xmonad $ … . setEwmhActivateHook doAskUrgent . ewmh . … $ def{…}
--
-- One may also wish to ignore activation requests from certain applications
-- entirely:
--
-- > import XMonad.Hooks.ManageHelpers
-- >
-- > myActivateHook :: ManageHook
-- > myActivateHook =
-- >   className /=? "Google-chrome" <&&> className /=? "google-chrome" --> doFocus
-- >
-- > main = xmonad $ … . setEwmhActivateHook myActivateHook . ewmh . … $ def{…}
--
-- Arbitrarily complex hooks can be used. This last example marks Chrome
-- windows as urgent and focuses everything else:
--
-- > myActivateHook :: ManageHook
-- > myActivateHook = composeOne
-- >   [ className =? "Google-chrome" <||> className =? "google-chrome" -?> doAskUrgent
-- >   , pure True -?> doFocus ]
--
-- See "XMonad.ManageHook", "XMonad.Hooks.ManageHelpers" and "XMonad.Hooks.Focus"
-- for functions that can be useful here.

-- | Set (replace) the hook which is invoked when a client sends a
-- @_NET_ACTIVE_WINDOW@ request to activate a window. The default is 'doFocus'
-- which focuses the window immediately, switching workspace if necessary.
-- 'XMonad.Hooks.UrgencyHook.doAskUrgent' is a less intrusive alternative.
--
-- More complex hooks can be constructed using combinators from
-- "XMonad.ManageHook", "XMonad.Hooks.ManageHelpers" and "XMonad.Hooks.Focus".
setEwmhActivateHook :: ManageHook -> XConfig l -> XConfig l
setEwmhActivateHook h = XC.modifyDef $ \c -> c{ activateHook = h }


-- $customFullscreen
-- When a client sends a @_NET_WM_STATE@ request to add/remove/toggle the
-- @_NET_WM_STATE_FULLSCREEN@ state, 'ewmhFullscreen' uses a pair of hooks to
-- make the window fullscreen and revert its state. The default hooks are
-- stateless: windows are fullscreened by turning them into fullscreen floats,
-- and reverted by sinking them into the tiling layer. This behaviour can be
-- configured by supplying a pair of 'ManageHook's to 'setEwmhFullscreenHooks'.
--
-- See "XMonad.Actions.ToggleFullFloat" for a pair of hooks that store the
-- original state of floating windows.

-- | Set (replace) the hooks invoked when clients ask to add/remove the
-- $_NET_WM_STATE_FULLSCREEN@ state. The defaults are 'doFullFloat' and
-- 'doSink'.
setEwmhFullscreenHooks :: ManageHook -> ManageHook -> XConfig l -> XConfig l
setEwmhFullscreenHooks f uf = XC.modifyDef $ \c -> c{ fullscreenHooks = (f, uf) }


-- $customManageDesktopViewport
-- Setting @_NET_DESKTOP_VIEWPORT@ is typically desired but can lead to a
-- confusing workspace list in polybar, where this information is used to
-- re-group the workspaces by monitor. See
-- <https://github.com/polybar/polybar/issues/2603 polybar#2603>.
--
-- To avoid this, you can use:
--
-- > main = xmonad $ … . disableEwmhManageDesktopViewport . ewmh . … $ def{…}
--
-- Note that if you apply this configuration in an already running environment,
-- the property may remain at its previous value. It can be removed by running:
--
-- > xprop -root -remove _NET_DESKTOP_VIEWPORT
--
-- Which should immediately fix your bar.
--
disableEwmhManageDesktopViewport :: XConfig l -> XConfig l
disableEwmhManageDesktopViewport = XC.modifyDef $ \c -> c{ manageDesktopViewport = False }


-- | Initializes EwmhDesktops and advertises EWMH support to the X server.
{-# DEPRECATED ewmhDesktopsStartup "Use ewmh instead." #-}
ewmhDesktopsStartup :: X ()
ewmhDesktopsStartup = setSupported

-- | Notifies pagers and window lists, such as those in the gnome-panel of the
-- current state of workspaces and windows.
{-# DEPRECATED ewmhDesktopsLogHook "Use ewmh instead." #-}
ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = XC.withDef ewmhDesktopsLogHook'

-- | Generalized version of ewmhDesktopsLogHook that allows an arbitrary
-- user-specified function to sort/filter the workspace list (post-sorting).
{-# DEPRECATED ewmhDesktopsLogHookCustom "Use ewmh and addEwmhWorkspaceSort instead." #-}
ewmhDesktopsLogHookCustom :: WorkspaceSort -> X ()
ewmhDesktopsLogHookCustom f =
    ewmhDesktopsLogHook' def{ workspaceSort = (f .) <$> workspaceSort def }

-- | Intercepts messages from pagers and similar applications and reacts on them.
--
-- Currently supports:
--
--  * _NET_CURRENT_DESKTOP (switching desktops)
--
--  * _NET_WM_DESKTOP (move windows to other desktops)
--
--  * _NET_ACTIVE_WINDOW (activate another window, changing workspace if needed)
--
--  * _NET_CLOSE_WINDOW (close window)
{-# DEPRECATED ewmhDesktopsEventHook "Use ewmh instead." #-}
ewmhDesktopsEventHook :: Event -> X All
ewmhDesktopsEventHook = XC.withDef . ewmhDesktopsEventHook'

-- | Generalized version of ewmhDesktopsEventHook that allows an arbitrary
-- user-specified function to sort/filter the workspace list (post-sorting).
{-# DEPRECATED ewmhDesktopsEventHookCustom "Use ewmh and addEwmhWorkspaceSort instead." #-}
ewmhDesktopsEventHookCustom :: WorkspaceSort -> Event -> X All
ewmhDesktopsEventHookCustom f e =
    ewmhDesktopsEventHook' e def{ workspaceSort = (f .) <$> workspaceSort def }

-- | Cached @_NET_DESKTOP_NAMES@, @_NET_NUMBER_OF_DESKTOPS@
newtype DesktopNames = DesktopNames [String] deriving Eq
instance ExtensionClass DesktopNames where initialValue = DesktopNames []

-- | Cached @_NET_CLIENT_LIST@
newtype ClientList = ClientList [Window] deriving Eq
instance ExtensionClass ClientList where initialValue = ClientList [none]

-- | Cached @_NET_CLIENT_LIST_STACKING@
newtype ClientListStacking = ClientListStacking [Window] deriving Eq
instance ExtensionClass ClientListStacking where initialValue = ClientListStacking [none]

-- | Cached @_NET_CURRENT_DESKTOP@
newtype CurrentDesktop = CurrentDesktop Int deriving Eq
instance ExtensionClass CurrentDesktop where initialValue = CurrentDesktop (complement 0)

-- | Cached @_NET_WM_DESKTOP@
newtype WindowDesktops = WindowDesktops (M.Map Window Int) deriving Eq
instance ExtensionClass WindowDesktops where initialValue = WindowDesktops (M.singleton none (complement 0))

-- | Cached @_NET_ACTIVE_WINDOW@
newtype ActiveWindow = ActiveWindow Window deriving Eq
instance ExtensionClass ActiveWindow where initialValue = ActiveWindow (complement none)

-- | Cached @_NET_DESKTOP_VIEWPORT@
newtype MonitorTags = MonitorTags [WorkspaceId] deriving (Show,Eq)
instance ExtensionClass MonitorTags where initialValue = MonitorTags []

-- | Compare the given value against the value in the extensible state. Run the
-- action if it has changed.
whenChanged :: (Eq a, ExtensionClass a) => a -> X () -> X ()
whenChanged = whenX . XS.modified . const

ewmhDesktopsLogHook' :: EwmhDesktopsConfig -> X ()
ewmhDesktopsLogHook' EwmhDesktopsConfig{workspaceSort, workspaceRename, manageDesktopViewport} = withWindowSet $ \s -> do
    sort' <- workspaceSort
    let ws = sort' $ W.workspaces s

    -- Set number of workspaces and names thereof
    rename <- workspaceRename
    let desktopNames = [ rename (W.tag w) w | w <- ws ]
    whenChanged (DesktopNames desktopNames) $ do
        setNumberOfDesktops (length desktopNames)
        setDesktopNames desktopNames

    -- Set client list which should be sorted by window age. We just
    -- guess that StackSet contains windows list in this order which
    -- isn't true but at least gives consistency with windows cycling
    let clientList = nub . concatMap (W.integrate' . W.stack) $ ws
    whenChanged (ClientList clientList) $ setClientList clientList

    -- @ws@ is sorted in the "workspace order", which, by default, is
    -- the lexicographical sorting on @WorkspaceId@.
    -- @_NET_CLIENT_LIST_STACKING@ is expected to be in the "bottom-to-top
    -- stacking order".  It is unclear what that would mean for windows on
    -- invisible workspaces, but it seems reasonable to assume that windows on
    -- the current workspace should be "at the top".  With the focused window to
    -- be the top most, meaning the last.
    --
    -- There has been a number of discussions on the order of windows within a
    -- workspace.  See:
    --
    --   https://github.com/xmonad/xmonad-contrib/issues/567
    --   https://github.com/xmonad/xmonad-contrib/pull/568
    --   https://github.com/xmonad/xmonad-contrib/pull/772
    let clientListStacking =
          let wsInFocusOrder = W.hidden s
                               ++ (map W.workspace . W.visible) s
                               ++ [W.workspace $ W.current s]
              stackWindows (W.Stack cur up down) = reverse up ++ down ++ [cur]
              workspaceWindows = maybe [] stackWindows . W.stack
              -- In case a window is a member of multiple workspaces, we keep
              -- only the last occurrence in the list.  One that is closer to
              -- the top in the focus order.
              uniqueKeepLast = reverse . nub . reverse
           in uniqueKeepLast $ concatMap workspaceWindows wsInFocusOrder
    whenChanged (ClientListStacking clientListStacking) $
      setClientListStacking clientListStacking

    -- Set current desktop number
    let current = W.currentTag s `elemIndex` map W.tag ws
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

    -- Set desktop Viewport
    when manageDesktopViewport $ do
        let visibleScreens = W.current s : W.visible s
            currentTags    = map (W.tag . W.workspace) visibleScreens
        whenChanged (MonitorTags currentTags) $ mkViewPorts s (map W.tag ws)

-- | Create the viewports from the current 'WindowSet' and a list of
-- already sorted workspace IDs.
mkViewPorts :: WindowSet -> [WorkspaceId] -> X ()
mkViewPorts winset = setDesktopViewport . concat . mapMaybe (viewPorts M.!?)
  where
    foc = W.current winset
    -- Hidden workspaces are mapped to the current screen's viewport.
    viewPorts :: M.Map WorkspaceId [Position]
    viewPorts = M.fromList $ map mkVisibleViewPort (foc : W.visible winset)
                          ++ map (mkViewPort foc)  (W.hidden winset)

    mkViewPort :: WindowScreen -> WindowSpace -> (WorkspaceId, [Position])
    mkViewPort scr w = (W.tag w, mkPos scr)

    mkVisibleViewPort :: WindowScreen -> (WorkspaceId, [Position])
    mkVisibleViewPort x = mkViewPort x (W.workspace x)

    mkPos :: WindowScreen -> [Position]
    mkPos scr = [rect_x (rect scr), rect_y (rect scr)]
      where rect = screenRect . W.screenDetail

ewmhDesktopsEventHook' :: Event -> EwmhDesktopsConfig -> X All
ewmhDesktopsEventHook'
        ClientMessageEvent{ev_window = w, ev_message_type = mt, ev_data = d}
        EwmhDesktopsConfig{workspaceSort, activateHook} =
    withWindowSet $ \s -> do
        sort' <- workspaceSort
        let ws = sort' $ W.workspaces s

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
                if W.peek s == Just w then mempty else windows $ W.focusWindow w
            | mt == a_aw -> do
                if W.peek s == Just w then mempty else windows . appEndo =<< runQuery activateHook w
            | mt == a_cw ->
                killWindow w
            | otherwise ->
                -- The Message is unknown to us, but that is ok, not all are meant
                -- to be handled by the window manager
                mempty

        mempty
ewmhDesktopsEventHook' _ _ = mempty

-- | A 'ManageHook' that shifts windows to the workspace they want to be in.
-- Useful for restoring browser windows to where they were before restart.
--
-- To only use this for browsers (which might be a good idea, as many apps try
-- to restore their window to their original position, but it's rarely
-- desirable outside of security updates of multi-window apps like a browser),
-- use this:
--
-- > stringProperty "WM_WINDOW_ROLE" =? "browser" --> ewmhDesktopsManageHook
ewmhDesktopsManageHook :: ManageHook
ewmhDesktopsManageHook = maybeToDefinite ewmhDesktopsMaybeManageHook

-- | 'ewmhDesktopsManageHook' as a 'MaybeManageHook' for use with
-- 'composeOne'. Returns 'Nothing' if the window didn't indicate any desktop
-- preference, otherwise 'Just' (even if the preferred desktop was out of
-- bounds).
ewmhDesktopsMaybeManageHook :: MaybeManageHook
ewmhDesktopsMaybeManageHook = desktop >>= traverse doShiftI
  where
    doShiftI :: Int -> ManageHook
    doShiftI d = do
        sort' <- liftX . XC.withDef $ \EwmhDesktopsConfig{workspaceSort} -> workspaceSort
        ws <- liftX . gets $ map W.tag . sort' . W.workspaces . windowset
        maybe idHook doShift $ ws !? d

-- | Add EWMH fullscreen functionality to the given config.
ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen c = c { startupHook     = startupHook c <> fullscreenStartup
                     , handleEventHook = handleEventHook c <> fullscreenEventHook }

-- | Advertises EWMH fullscreen support to the X server.
{-# DEPRECATED fullscreenStartup "Use ewmhFullscreen instead." #-}
fullscreenStartup :: X ()
fullscreenStartup = setFullscreenSupported

-- | An event hook to handle applications that wish to fullscreen using the
-- @_NET_WM_STATE@ protocol. This includes users of the @gtk_window_fullscreen()@
-- function, such as Totem, Evince and OpenOffice.org.
--
-- Note this is not included in 'ewmh'.
{-# DEPRECATED fullscreenEventHook "Use ewmhFullscreen instead." #-}
fullscreenEventHook :: Event -> X All
fullscreenEventHook = XC.withDef . fullscreenEventHook'

fullscreenEventHook' :: Event -> EwmhDesktopsConfig -> X All
fullscreenEventHook'
    ClientMessageEvent{ev_event_display = dpy, ev_window = win, ev_message_type = typ, ev_data = action:dats}
    EwmhDesktopsConfig{fullscreenHooks = (fullscreenHook, unFullscreenHook)} = do
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
      windows . appEndo =<< runQuery fullscreenHook win
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      windows . appEndo =<< runQuery unFullscreenHook win

  return $ All True

fullscreenEventHook' _ _ = return $ All True

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
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST"
    io $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fromIntegral wins)

setClientListStacking :: [Window] -> X ()
setClientListStacking wins = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_CLIENT_LIST_STACKING"
    io $ changeProperty32 dpy r a wINDOW propModeReplace (fmap fromIntegral wins)

setWindowDesktop :: (Integral a) => Window -> a -> X ()
setWindowDesktop win i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_DESKTOP"
    io $ changeProperty32 dpy win a cARDINAL propModeReplace [fromIntegral i]

setActiveWindow :: Window -> X ()
setActiveWindow w = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_ACTIVE_WINDOW"
    io $ changeProperty32 dpy r a wINDOW propModeReplace [fromIntegral w]

setDesktopViewport :: [Position] -> X ()
setDesktopViewport positions = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- io $ internAtom dpy "_NET_DESKTOP_VIEWPORT" True
    io $ changeProperty32 dpy r a cARDINAL propModeReplace (map fi positions)

setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_DEMANDS_ATTENTION"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ,"_NET_WM_STRUT_PARTIAL"
                         ,"_NET_DESKTOP_VIEWPORT"
                         ]
    io $ changeProperty32 dpy r a aTOM propModeReplace (fmap fromIntegral supp)

    setWMName "xmonad"

-- TODO: use in SetWMName, UrgencyHook
addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
        supportedList <- join . maybeToList <$> getWindowProperty32 dpy a r
        changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)

setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]
