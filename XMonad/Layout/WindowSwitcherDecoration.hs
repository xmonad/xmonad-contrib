{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowSwitcherDecoration
-- Description :  Switch the position of windows by dragging them onto each other.
-- Copyright   :  (c) Jan Vornberger 2009
--                    Alejandro Serrano 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A decoration that allows to switch the position of windows by dragging
-- them onto each other.
--
-----------------------------------------------------------------------------

module XMonad.Layout.WindowSwitcherDecoration
    ( -- * Usage:
      -- $usage
      windowSwitcherDecoration,
      windowSwitcherDecorationWithButtons,
      windowSwitcherDecorationWithImageButtons,
      WindowSwitcherDecoration, ImageWindowSwitcherDecoration,
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.DraggingVisualizer
import qualified XMonad.StackSet as S
import XMonad.Prelude
import Foreign.C.Types(CInt)

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WindowSwitcherDecoration
-- > import XMonad.Layout.DraggingVisualizer
--
-- Then edit your @layoutHook@ by adding the WindowSwitcherDecoration to
-- your layout:
--
-- > myL = windowSwitcherDecoration shrinkText def (draggingVisualizer $ layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- There is also a version of the decoration that contains buttons like
-- "XMonad.Layout.ButtonDecoration". To use that version, you will need to
-- import "XMonad.Layout.DecorationAddons" as well and modify your @layoutHook@
-- in the following way:
--
-- > import XMonad.Layout.DecorationAddons
-- >
-- > myL = windowSwitcherDecorationWithButtons shrinkText defaultThemeWithButtons (draggingVisualizer $ layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--
-- Additionaly, there is a version of the decoration that contains image buttons like
-- "XMonad.Layout.ImageButtonDecoration". To use that version, you will need to
-- import "XMonad.Layout.ImageButtonDecoration" as well and modify your @layoutHook@
-- in the following way:
--
-- > import XMonad.Layout.ImageButtonDecoration
-- >
-- > myL = windowSwitcherDecorationWithImageButtons shrinkText defaultThemeWithImageButtons (draggingVisualizer $ layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--

windowSwitcherDecoration :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration WindowSwitcherDecoration s) l a
windowSwitcherDecoration s c = decoration s c $ WSD False

windowSwitcherDecorationWithButtons :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration WindowSwitcherDecoration s) l a
windowSwitcherDecorationWithButtons s c = decoration s c $ WSD True

newtype WindowSwitcherDecoration a = WSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle WindowSwitcherDecoration a where
    describeDeco _ = "WindowSwitcherDeco"

    decorationCatchClicksHook (WSD withButtons) mainw dFL dFR = if withButtons
                                                                    then titleBarButtonHandler mainw dFL dFR
                                                                    else return False
    decorationWhileDraggingHook _ = handleTiledDraggingInProgress
    decorationAfterDraggingHook _ (mainw, _) decoWin = do focus mainw
                                                          hasCrossed <- handleScreenCrossing mainw decoWin
                                                          unless hasCrossed $ do sendMessage DraggingStopped
                                                                                 performWindowSwitching mainw

-- Note: the image button code is duplicated from the above
-- because the title bar handle is different

windowSwitcherDecorationWithImageButtons :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration ImageWindowSwitcherDecoration s) l a
windowSwitcherDecorationWithImageButtons s c = decoration s c $ IWSD True

newtype ImageWindowSwitcherDecoration a = IWSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageWindowSwitcherDecoration a where
    describeDeco _ = "ImageWindowSwitcherDeco"

    decorationCatchClicksHook (IWSD withButtons) mainw dFL dFR = if withButtons
                                                                    then imageTitleBarButtonHandler mainw dFL dFR
                                                                    else return False
    decorationWhileDraggingHook _ = handleTiledDraggingInProgress
    decorationAfterDraggingHook _ (mainw, _) decoWin = do focus mainw
                                                          hasCrossed <- handleScreenCrossing mainw decoWin
                                                          unless hasCrossed $ do sendMessage DraggingStopped
                                                                                 performWindowSwitching mainw

handleTiledDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleTiledDraggingInProgress ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ DraggingWindow mainw rect

performWindowSwitching :: Window -> X ()
performWindowSwitching win =
    withDisplay $ \d -> do
       root <- asks theRoot
       (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
       ws <- gets windowset
       let allWindows = S.index ws
       -- do a little double check to be sure
       when ((win `elem` allWindows) && (selWin `elem` allWindows)) $ do
                let allWindowsSwitched = map (switchEntries win selWin) allWindows
                let (ls, notEmpty -> t :| rs) = break (win ==) allWindowsSwitched
                let newStack = S.Stack t (reverse ls) rs
                windows $ S.modify' $ const newStack
    where
        switchEntries a b x
            | x == a    = b
            | x == b    = a
            | otherwise = x
