-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.FadeInactive
-- Copyright    : (c) 2008 Justin Bogner <mail@justinbogner.com>
-- License      : BSD
--
-- Maintainer   : Justin Bogner <mail@justinbogner.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Makes XMonad set the _NET_WM_WINDOW_OPACITY atom for inactive windows,
-- which causes those windows to become slightly translucent if something
-- like xcompmgr is running
-----------------------------------------------------------------------------
module XMonad.Hooks.FadeInactive (
    -- * Usage
    -- $usage
    fadeInactiveLogHook
    ) where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad (forM_)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.FadeInactive
-- >
-- > myLogHook :: X ()
-- > myLogHook = fadeInactiveLogHook fadeAmount
-- >     where fadeAmount = 0xdddddddd
-- >
-- > main = xmonad defaultConfig { logHook = myLogHook }
--
-- fadeAmount can be any integer
-- you will need to have xcompmgr <http://freedesktop.org/wiki/Software/xapps>
-- or something similar for this to do anything
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- |
-- sets the opacity of a window
setOpacity :: Window -> Integer -> X ()
setOpacity w t = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy w a c propModeReplace [fromIntegral t]

-- |
-- fades a window out by setting the opacity
fadeOut :: Integer -> Window -> X ()
fadeOut amt = flip setOpacity amt

-- |
-- makes a window completely opaque
fadeIn :: Window -> X ()
fadeIn = flip setOpacity 0xffffffff

-- |
-- lowers the opacity of inactive windows to the specified amount
fadeInactiveLogHook :: Integer -> X ()
fadeInactiveLogHook amt = withWindowSet $ \s ->
    forM_ (concatMap visibleWins $ W.current s : W.visible s) (fadeOut amt) >>
    withFocused fadeIn
        where
          visibleWins = maybe [] unfocused . W.stack . W.workspace
          unfocused (W.Stack _ l r) = l ++ r
