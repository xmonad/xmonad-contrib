----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationAddons
-- Description :  Various stuff that can be added to the decoration.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Various stuff that can be added to the decoration. Most of it
-- is intended to be used by other modules. See
-- "XMonad.Layout.ButtonDecoration" for a module that makes use of this.
--
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationAddons (
                                    titleBarButtonHandler
                                   ,defaultThemeWithButtons
                                   ,handleScreenCrossing
                                   ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Actions.WindowMenu
import XMonad.Actions.Minimize
import XMonad.Layout.Maximize
import XMonad.Hooks.ManageDocks
import XMonad.Util.Font
import XMonad.Util.PositionStore

import XMonad.Prelude
import qualified Data.Set as S

minimizeButtonOffset :: Int
minimizeButtonOffset = 48

maximizeButtonOffset :: Int
maximizeButtonOffset = 25

closeButtonOffset :: Int
closeButtonOffset = 10

buttonSize :: Int
buttonSize = 10

-- | A function intended to be plugged into the 'decorationCatchClicksHook' of a decoration.
-- It will intercept clicks on the buttons of the decoration and invoke the associated action.
-- To actually see the buttons, you will need to use a theme that includes them.
-- See 'defaultThemeWithButtons' below.
titleBarButtonHandler :: Window -> Int -> Int -> X Bool
titleBarButtonHandler mainw distFromLeft distFromRight = do
    let action
          | fi distFromLeft <= 3 * buttonSize = focus mainw >> windowMenu >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | fi distFromRight >= maximizeButtonOffset &&
            fi distFromRight <= maximizeButtonOffset + (2 * buttonSize) = focus mainw >> sendMessage (maximizeRestore mainw) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

-- | Intended to be used together with 'titleBarButtonHandler'. See above.
defaultThemeWithButtons :: Theme
defaultThemeWithButtons = def {
                            windowTitleAddons = [ (" (M)", AlignLeft)
                                                , ("_"   , AlignRightOffset minimizeButtonOffset)
                                                , ("[]"  , AlignRightOffset maximizeButtonOffset)
                                                , ("X"   , AlignRightOffset closeButtonOffset)
                                                ]
                            }

-- | A function intended to be plugged into the 'decorationAfterDraggingHook' of a decoration.
-- It will check if the window has been dragged onto another screen and shift it there.
-- The PositionStore is also updated accordingly, as this is designed to be used together
-- with "XMonad.Layout.PositionStoreFloat".
handleScreenCrossing :: Window -> Window -> X Bool
handleScreenCrossing w decoWin = withDisplay $ \d -> do
    root <- asks theRoot
    (_, _, _, px, py, _, _, _) <- io $ queryPointer d root
    ws <- gets windowset
    sc <- fromMaybe (W.current ws) <$> pointScreen (fi px) (fi py)
    maybeWksp <- screenWorkspace $ W.screen sc
    let targetWksp = maybeWksp >>= \wksp ->
                        W.findTag w ws >>= \currentWksp ->
                        if currentWksp /= wksp
                            then Just wksp
                            else Nothing
    case targetWksp of
        Just wksp -> do
                        -- find out window under cursor on target workspace
                        -- apparently we have to switch to the workspace first
                        -- to make this work, which unforunately introduces some flicker
                        windows $ \ws' -> W.view wksp ws'
                        (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root

                        -- adjust PositionStore
                        let oldScreenRect = screenRect . W.screenDetail $ W.current ws
                            newScreenRect = screenRect . W.screenDetail $ sc
                        {-- somewhat ugly hack to get proper ScreenRect,
                            creates unwanted inter-dependencies
                            TODO: get ScreenRects in a proper way --}
                        oldScreenRect' <- fmap ($ oldScreenRect) (calcGap $ S.fromList [minBound .. maxBound])
                        newScreenRect' <- fmap ($ newScreenRect) (calcGap $ S.fromList [minBound .. maxBound])
                        wa <- io $ getWindowAttributes d decoWin
                        modifyPosStore (\ps ->
                            posStoreMove ps w (fi $ wa_x wa) (fi $ wa_y wa)
                                oldScreenRect' newScreenRect')

                        -- set focus correctly so the window will be inserted
                        -- at the correct position on the target workspace
                        -- and then shift the window
                        windows $ \ws' -> W.shiftWin wksp w . W.focusWindow selWin $ ws'

                        -- return True to signal that screen crossing has taken place
                        return True
        Nothing -> return False
