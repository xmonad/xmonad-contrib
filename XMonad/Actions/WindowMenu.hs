----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowMenu
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
import XMonad.Layout.Minimize
import XMonad.Util.XUtils (fi)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.WindowMenu
--
-- Then add a keybinding, e.g.
--
-- >    , ((modMask x,               xK_o ), windowMenu)

windowMenu :: X ()
windowMenu = withFocused $ \w -> do
    tags <- asks (workspaces . config)
    Rectangle x y wh ht <- getSize w
    Rectangle sx sy swh sht <- gets $ screenRect . W.screenDetail . W.current . windowset
    let originFractX = (fi x - fi sx + fi wh / 2) / fi swh
        originFractY = (fi y - fi sy + fi ht / 2) / fi sht
        gsConfig = defaultGSConfig
                    { gs_originFractX = originFractX
                    , gs_originFractY = originFractY }
        actions = [ ("Cancel menu", return ())
                  , ("Close"      , kill)
                  , ("Maximize"   , sendMessage $ maximizeRestore w)
                  , ("Minimize"   , sendMessage $ MinimizeWin w)
                  ] ++
                  [ ("Move to " ++ tag, windows $ W.shift tag)
                    | tag <- tags ]
    runSelectedAction gsConfig actions

getSize :: Window -> X (Rectangle)
getSize w = do
  d  <- asks display
  wa <- io $ getWindowAttributes d w
  let x = fi $ wa_x wa
      y = fi $ wa_y wa
      wh = fi $ wa_width wa
      ht = fi $ wa_height wa
  return (Rectangle x y wh ht)
