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
    ewmh,
    ewmhDesktopsStartup,
    ewmhDesktopsLogHook,
    ewmhDesktopsLogHookCustom,
    ewmhDesktopsEventHook,
    ewmhDesktopsEventHookCustom,
    fullscreenEventHook
    ) where

import Codec.Binary.UTF8.String (encode)
import Data.List
import Data.Maybe
import Data.Monoid

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName
import XMonad.Util.XUtils (fi)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties (getProp32)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops
-- >
-- > main = xmonad $ ewmh defaultConfig{ handleEventHook =
-- >            handleEventHook defaultConfig <+> fullscreenEventHook }
--
-- You may also be interested in 'avoidStruts' from "XMonad.Hooks.ManageDocks".


-- | Add EWMH functionality to the given config.  See above for an example.
ewmh :: XConfig a -> XConfig a
ewmh c = c { startupHook     = startupHook c +++ ewmhDesktopsStartup
           , handleEventHook = handleEventHook c +++ ewmhDesktopsEventHook
           , logHook         = logHook c +++ ewmhDesktopsLogHook }
 where x +++ y = mappend x y

-- |
-- Initializes EwmhDesktops and advertises EWMH support to the X
-- server
ewmhDesktopsStartup :: X ()
ewmhDesktopsStartup = setSupported

-- |
-- Notifies pagers and window lists, such as those in the gnome-panel
-- of the current state of workspaces and windows.
ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = ewmhDesktopsLogHookCustom id
-- |
-- Generalized version of ewmhDesktopsLogHook that allows an arbitrary
-- user-specified function to transform the workspace list (post-sorting)
ewmhDesktopsLogHookCustom :: ([WindowSpace] -> [WindowSpace]) -> X ()
ewmhDesktopsLogHookCustom f = withWindowSet $ \s -> do
    sort' <- getSortByIndex
    let ws = f $ sort' $ W.workspaces s

    -- Number of Workspaces
    setNumberOfDesktops (length ws)

    -- Names thereof
    setDesktopNames (map W.tag ws)

    -- all windows, with focused windows last
    let wins =  nub . concatMap (maybe [] (\(W.Stack x l r)-> reverse l ++ r ++ [x]) . W.stack) $ ws
    setClientList wins

    -- Current desktop
    case (elemIndex (W.currentTag s) $ map W.tag ws) of
      Nothing -> return ()
      Just curr -> do
        setCurrentDesktop curr

        -- Per window Desktop
        -- To make gnome-panel accept our xinerama stuff, we display
        -- all visible windows on the current desktop.
        forM_ (W.current s : W.visible s) $ \x ->
            forM_ (W.integrate' (W.stack (W.workspace x))) $ \win -> do
                setWindowDesktop win curr

    forM_ (W.hidden s) $ \w ->
        case elemIndex (W.tag w) (map W.tag ws) of
          Nothing -> return ()
          Just wn -> forM_ (W.integrate' (W.stack w)) $ \win -> do
                         setWindowDesktop win wn

    setActiveWindow

    return ()

-- |
-- Intercepts messages from pagers and similar applications and reacts on them.
-- Currently supports:
--
--  * _NET_CURRENT_DESKTOP (switching desktops)
--
--  * _NET_WM_DESKTOP (move windows to other desktops)
--
--  * _NET_ACTIVE_WINDOW (activate another window, changing workspace if needed)
ewmhDesktopsEventHook = ewmhDesktopsEventHookCustom id

-- |
-- Generalized version of ewmhDesktopsEventHook that allows an arbitrary
-- user-specified function to transform the workspace list (post-sorting)
ewmhDesktopsEventHookCustom :: ([WindowSpace] -> [WindowSpace]) -> Event -> X All
ewmhDesktopsEventHookCustom f e = handle f e >> return (All True)

handle :: ([WindowSpace] -> [WindowSpace]) -> Event -> X ()
handle f (ClientMessageEvent {
               ev_window = w,
               ev_message_type = mt,
               ev_data = d
       }) = withWindowSet $ \s -> do
       sort' <- getSortByIndex
       let ws = f $ sort' $ W.workspaces s

       a_cd <- getAtom "_NET_CURRENT_DESKTOP"
       a_d <- getAtom "_NET_WM_DESKTOP"
       a_aw <- getAtom "_NET_ACTIVE_WINDOW"
       a_cw <- getAtom "_NET_CLOSE_WINDOW"
       a_ignore <- mapM getAtom ["XMONAD_TIMER"]
       if  mt == a_cd then do
               let n = head d
               if 0 <= n && fi n < length ws then
                       windows $ W.view (W.tag (ws !! fi n))
                 else  trace $ "Bad _NET_CURRENT_DESKTOP with data[0]="++show n
        else if mt == a_d then do
               let n = head d
               if 0 <= n && fi n < length ws then
                       windows $ W.shiftWin (W.tag (ws !! fi n)) w
                 else  trace $ "Bad _NET_DESKTOP with data[0]="++show n
        else if mt == a_aw then do
               windows $ W.focusWindow w
        else if mt == a_cw then do
               killWindow w
        else if mt `elem` a_ignore then do
           return ()
        else do
          -- The Message is unknown to us, but that is ok, not all are meant
          -- to be handled by the window manager
          return ()
handle _ _ = return ()

-- |
-- An event hook to handle applications that wish to fullscreen using the
-- _NET_WM_STATE protocol. This includes users of the gtk_window_fullscreen()
-- function, such as Totem, Evince and OpenOffice.org.
--
-- Note this is not included in 'ewmh'.
fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 wmstate win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      ptype = 4 -- The atom property type for changeProperty
      chWstate f = io $ changeProperty32 dpy win wmstate ptype propModeReplace (f wstate)

  when (typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      windows $ W.sink win

  return $ All True

fullscreenEventHook _ = return $ All True

setNumberOfDesktops :: (Integral a) => a -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    c <- getAtom "CARDINAL"
    r <- asks theRoot
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral n]

setCurrentDesktop :: (Integral a) => a -> X ()
setCurrentDesktop i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_CURRENT_DESKTOP"
    c <- getAtom "CARDINAL"
    r <- asks theRoot
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral i]

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
    c <- getAtom "WINDOW"
    a <- getAtom "_NET_CLIENT_LIST"
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral wins)
    a' <- getAtom "_NET_CLIENT_LIST_STACKING"
    io $ changeProperty32 dpy r a' c propModeReplace (fmap fromIntegral wins)

setWindowDesktop :: (Integral a) => Window -> a -> X ()
setWindowDesktop win i = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_DESKTOP"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy win a c propModeReplace [fromIntegral i]

setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

    setWMName "xmonad"

setActiveWindow :: X ()
setActiveWindow = withWindowSet $ \s -> withDisplay $ \dpy -> do
    let w = fromMaybe none (W.peek s)
    r <- asks theRoot
    a <- getAtom "_NET_ACTIVE_WINDOW"
    c <- getAtom "WINDOW"
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral w]
