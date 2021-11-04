{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ButtonDecoration
-- Description :  Decoration that includes buttons, executing actions when clicked on.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A decoration that includes small buttons on both ends which invoke
-- various actions when clicked on: Show a window menu (see
-- "XMonad.Actions.WindowMenu"), minimize, maximize or close the window.
--
-- Note: For maximizing and minimizing to actually work, you will need
-- to integrate "XMonad.Layout.Maximize" and "XMonad.Layout.Minimize" into your
-- setup.  See the documentation of those modules for more information.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ButtonDecoration
    ( -- * Usage:
      -- $usage
      buttonDeco,
      ButtonDecoration,
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DecorationAddons
-- > import XMonad.Layout.ButtonDecoration
--
-- Then edit your @layoutHook@ by adding the ButtonDecoration to
-- your layout:
--
-- > myL = buttonDeco shrinkText defaultThemeWithButtons (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--

buttonDeco :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration ButtonDecoration s) l a
buttonDeco s c = decoration s c $ NFD True

newtype ButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ButtonDecoration a where
    describeDeco _ = "ButtonDeco"
    decorationCatchClicksHook _ = titleBarButtonHandler
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()
