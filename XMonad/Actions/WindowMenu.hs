----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowMenu
-- Description :  Display window management actions in the center of the focused window.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Uses "XMonad.Actions.GridSelect" to display a number of actions related to
-- window management in the center of the focused window. Actions include: Closing,
-- maximizing, minimizing and shifting the window to another workspace.
--
-- Note: For maximizing and minimizing to actually work, you will need
-- to integrate "XMonad.Layout.Maximize" and "XMonad.Layout.Minimize" into your
-- setup.  See the documentation of those modules for more information.
--
-----------------------------------------------------------------------------

module XMonad.Actions.WindowMenu (
                             -- * Usage
                             -- $usage
                             windowMenu
                              ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import XMonad.Layout.Maximize
import XMonad.Actions.Minimize
import XMonad.Prelude (fi)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.WindowMenu
--
-- Then add a keybinding, e.g.
--
-- >    , ((modm,               xK_o ), windowMenu)

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
    fBC <- asks (focusedBorderColor . config)
    nBC <- asks (normalBorderColor . config)
    return $ if isFg
                then (fBC, nBC)
                else (nBC, fBC)

windowMenu :: X ()
windowMenu = withFocused $ \w -> withDisplay $ \d -> withWindowAttributes d w $ \wa -> do
    tags <- asks (workspaces . config)
    let Rectangle x y wh ht = getSize wa
    Rectangle sx sy swh sht <- gets $ screenRect . W.screenDetail . W.current . windowset
    let originFractX = (fi x - fi sx + fi wh / 2) / fi swh
        originFractY = (fi y - fi sy + fi ht / 2) / fi sht
        gsConfig = (buildDefaultGSConfig colorizer)
                    { gs_originFractX = originFractX
                    , gs_originFractY = originFractY }
        actions = [ ("Cancel menu", return ())
                  , ("Close"      , kill)
                  , ("Maximize"   , sendMessage $ maximizeRestore w)
                  , ("Minimize"   , minimizeWindow w)
                  ] ++
                  [ ("Move to " ++ tag, windows $ W.shift tag)
                    | tag <- tags ]
    runSelectedAction gsConfig actions

getSize :: WindowAttributes -> Rectangle
getSize wa =
  let x = fi $ wa_x wa
      y = fi $ wa_y wa
      wh = fi $ wa_width wa
      ht = fi $ wa_height wa
   in Rectangle x y wh ht
