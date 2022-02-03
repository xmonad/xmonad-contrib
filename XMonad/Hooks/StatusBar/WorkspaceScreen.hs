{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{- |
 Module      :  XMonad.Hooks.StatusBar.WorkspaceScreen
 Description :  Combine workspace names with screen information
 Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
 License     :  BSD3-style (see LICENSE)

 Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
 Stability   :  unstable
 Portability :  unportable

 In multi-head setup, it might be useful to have screen information of the
 visible workspaces combined with the workspace name, for example in a status
 bar. This module provides utility functions to do just that.
-}
module XMonad.Hooks.StatusBar.WorkspaceScreen
    (
    -- * Usage
    -- $usage
      combineWithScreen
    , combineWithScreenName
    , combineWithScreenNumber
    , WorkspaceScreenCombiner
    -- * Limitations
    -- $limitations
    ) where

import           Graphics.X11.Xrandr
import           XMonad
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Prelude
import qualified XMonad.StackSet               as W

{- $usage
 You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:

 > import XMonad
 > import XMonad.Hooks.StatusBar
 > import XMonad.Hooks.StatusBar.PP
 > import XMonad.Hooks.StatusBar.WorkspaceScreen

 For example, to add the screen number in parentheses to each visible
 workspace number, you can use 'combineWithScreenNumber':

 > myWorkspaceScreenCombiner :: WorkspaceId -> String -> String
 > myWorkspaceScreenCombiner w sc = w <> wrap "(" ")" sc
 >
 > mySB = statusBarProp "xmobar" (combineWithScreenNumber myWorkspaceScreenCombiner xmobarPP)
 > main = xmonad $ withEasySB mySB defToggleStrutsKey def

 This will annotate the workspace names as following:

 > [1(0)] 2 3 4 <5(1)> 6 7 8 9

 To use the screen's name instead, checkout 'combineWithScreenName':

 > [1(eDP-1)] 2 3 4 <5(HDMI-1)> 6 7 8 9

 For advanced cases, use 'combineWithScreen'.
-}

{- $limitations
 For simplicity, this module assumes xmonad screen ids match screen/monitor
 numbers as managed by the X server (for example, as given by @xrandr
 --listactivemonitors@). Thus, it may not work well when screens show an
 overlapping range of the framebuffer, e.g. when using a projector. This also
 means that it doesn't work with "XMonad.Layout.LayoutScreens".
 (This isn't difficult to fix, PRs welcome.)
-}

-- | Type synonym for a function that combines a workspace name with a screen.
type WorkspaceScreenCombiner = WorkspaceId -> WindowScreen -> String

-- | A helper function that returns a list of screen names.
screenNames :: X [Maybe String]
screenNames = do
    XConf { display, theRoot } <- ask
    let getName mi = getAtomName display (xrr_moninf_name mi)
    io
        $   maybe (pure []) (traverse getName)
        =<< xrrGetMonitors display theRoot True

-- | Combine a workspace name with the screen name it's visible on.
combineWithScreenName :: (WorkspaceId -> String -> String) -> PP -> X PP
combineWithScreenName c = combineWithScreen $ do
    screens <- screenNames
    return $ \w sc -> maybe w (c w) $ join (screens !? fi (W.screen sc))

-- | Combine a workspace name with the screen number it's visible on.
combineWithScreenNumber :: (WorkspaceId -> String -> String) -> PP -> X PP
combineWithScreenNumber c =
    combineWithScreen . return $ \w sc -> c w (show @Int . fi . W.screen $ sc)

-- | Combine a workspace name with a screen according to the given
-- 'WorkspaceScreenCombiner'.
combineWithScreen :: X WorkspaceScreenCombiner -> PP -> X PP
combineWithScreen xCombiner pp = do
    combiner <- xCombiner
    ss       <- withWindowSet (return . W.screens)
    return $ pp
        { ppRename = ppRename pp <=< \s w ->
            maybe s (combiner s) (find ((== W.tag w) . W.tag . W.workspace) ss)
        }
