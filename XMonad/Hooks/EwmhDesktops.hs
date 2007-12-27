-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.EwmhDesktops
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes xmonad use the EWMH hints to tell panel applications about its
-- workspaces and the windows therein.
-----------------------------------------------------------------------------
module XMonad.Hooks.EwmhDesktops (
    -- * Usage
    -- $usage
    ewmhDesktopsLogHook
    ) where

import Data.List
import Data.Maybe

import XMonad
import Control.Monad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName
import XMonad.Util.WorkspaceCompare

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops
-- >
-- > myLogHook :: X ()
-- > myLogHook = do ewmhDesktopsLogHook
-- >                return ()
-- >
-- > main = xmonad defaultConfig { logHook = myLogHook }
-- 
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"

-- | 
-- Notifies pagers and window lists, such as those in the gnome-panel
-- of the current state of workspaces and windows.
ewmhDesktopsLogHook :: X ()
ewmhDesktopsLogHook = withWindowSet $ \s -> do
    sort' <- getSortByTag
    let ws = sort' $ W.workspaces s
    let wins = W.allWindows s

    setSupported

    -- Number of Workspaces
    setNumberOfDesktops (length ws)

    -- Names thereof
    setDesktopNames (map W.tag ws)

    -- Current desktop
    let curr = fromJust $ elemIndex (W.tag (W.workspace (W.current s))) $ map W.tag ws
    
    setCurrentDesktop curr

    setClientList wins

    -- Per window Desktop
    -- To make gnome-panel accept our xinerama stuff, we display
    -- all visible windows on the current desktop.
    forM_ (W.current s : W.visible s) $ \s -> 
        forM_ (W.integrate' (W.stack (W.workspace s))) $ \win -> do
            setWindowDesktop win curr

    forM_ (W.hidden s) $ \w -> 
        let wn = fromJust $ elemIndex (W.tag w) (map W.tag ws) in
        forM_ (W.integrate' (W.stack w)) $ \win -> do
            setWindowDesktop win wn

    setActiveWindow

    return ()


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
    let names' = map (fromIntegral.fromEnum) $
            concatMap (++['\0']) names
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
    let w = fromMaybe 0 (W.peek s)
    r <- asks theRoot
    a <- getAtom "_NET_ACTIVE_WINDOW"
    c <- getAtom "WINDOW"
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral w]
