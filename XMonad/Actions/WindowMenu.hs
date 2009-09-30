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
-- Uses XMonad.Actions.GridSelect to display a number of actions related to
-- window management in the center of the focused window. Actions include: Closing,
-- maximizing, minimizing and shifting the window to another workspace.
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

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.WindowMenu
--
-- Then add a keybinding, e.g.
--
-- >    , ((modMask x,               xK_o ), windowMenu)

simpleColorizer :: (Monad m) => t -> t -> t1 -> Bool -> m (t, [Char])
simpleColorizer nBC _ _ False  = return (nBC, "black")
simpleColorizer _ fBC _ True  = return (fBC, "black")

windowMenu :: X ()
windowMenu = withFocused $ \w -> do
    nBC <- asks (normalBorderColor . config)
    fBC <- asks (focusedBorderColor . config)
    tags <- asks (workspaces . config)
    Rectangle x y wh ht <- getSize w
    Rectangle sx sy swh sht <- gets $ screenRect . W.screenDetail . W.current . windowset
    let originFractX = (fromIntegral x - fromIntegral sx + (fromIntegral wh / 2))
                        / fromIntegral swh
        originFractY = (fromIntegral y -fromIntegral sy + (fromIntegral ht / 2))
                        / fromIntegral sht
        colorizer = simpleColorizer nBC fBC
        gsConfig = buildDefaultGSConfig colorizer
        gsConfig' = gsConfig { gs_originFractX = originFractX,
                               gs_originFractY = originFractY }
        actions = [ ("Cancel menu", return ())
                  , ("Close"      , kill)
                  , ("Maximize"   , sendMessage $ maximizeRestore w)
                  , ("Minimize"   , sendMessage $ MinimizeWin w)
                  ] ++
                  [ ("Move to " ++ tag, windows $ W.shift tag)
                    | tag <- tags ]
    runSelectedAction gsConfig' actions

getSize :: Window -> X (Rectangle)
getSize w = do
  d  <- asks display
  wa <- io $ getWindowAttributes d w
  let x = fromIntegral $ wa_x wa
      y = fromIntegral $ wa_y wa
      wh = fromIntegral $ wa_width wa
      ht = fromIntegral $ wa_height wa
  return (Rectangle x y wh ht)
