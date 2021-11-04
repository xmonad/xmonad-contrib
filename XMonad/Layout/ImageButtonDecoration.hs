{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ImageButtonDecoration
-- Description :  Decoration that includes image buttons, executing actions when clicked on.
-- Copyright   :  (c) Jan Vornberger 2009
--                    Alejandro Serrano 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  trupill@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A decoration that includes small image buttons on both ends which invoke
-- various actions when clicked on: Show a window menu (see
-- "XMonad.Actions.WindowMenu"), minimize, maximize or close the window.
--
-- Note: For maximizing and minimizing to actually work, you will need
-- to integrate "XMonad.Layout.Maximize" and "XMonad.Layout.Minimize" into your
-- setup.  See the documentation of those modules for more information.
--
-----------------------------------------------------------------------------

-- This module is mostly derived from "XMonad.Layout.DecorationAddons"
-- and "XMonad.Layout.ButtonDecoration"

module XMonad.Layout.ImageButtonDecoration
    ( -- * Usage:
      -- $usage
      imageButtonDeco
    , defaultThemeWithImageButtons
    , imageTitleBarButtonHandler
    , ImageButtonDecoration
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Util.Image

import XMonad.Actions.WindowMenu
import XMonad.Actions.Minimize
import XMonad.Layout.Maximize

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ImageButtonDecoration
--
-- Then edit your @layoutHook@ by adding the ImageButtonDecoration to
-- your layout:
--
-- > myL = imageButtonDeco shrinkText defaultThemeWithImageButtons (layoutHook def)
-- > main = xmonad def { layoutHook = myL }
--

-- The buttons' dimension and placements

buttonSize :: Int
buttonSize = 10

menuButtonOffset :: Int
menuButtonOffset = 4

minimizeButtonOffset :: Int
minimizeButtonOffset = 32

maximizeButtonOffset :: Int
maximizeButtonOffset = 18

closeButtonOffset :: Int
closeButtonOffset = 4


-- The images in a 0-1 scale to make
-- it easier to visualize

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (== 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,0,0,0,0,0,0,1,1],
               [1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[1,1,0,0,0,0,0,0,1,1],
                [1,1,1,0,0,0,0,1,1,1],
                [0,1,1,1,0,0,1,1,1,0],
                [0,0,1,1,1,1,1,1,0,0],
                [0,0,0,1,1,1,1,0,0,0],
                [0,0,0,1,1,1,1,0,0,0],
                [0,0,1,1,1,1,1,1,0,0],
                [0,1,1,1,0,0,1,1,1,0],
                [1,1,1,0,0,0,0,1,1,1],
                [1,1,0,0,0,0,0,0,1,1]]


closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

-- | A function intended to be plugged into the 'decorationCatchClicksHook' of a decoration.
-- It will intercept clicks on the buttons of the decoration and invoke the associated action.
-- To actually see the buttons, you will need to use a theme that includes them.
-- See 'defaultThemeWithImageButtons' below.
imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
    let action
          | fi distFromLeft >= menuButtonOffset &&
             fi distFromLeft <= menuButtonOffset + buttonSize = focus mainw >> windowMenu >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | fi distFromRight >= maximizeButtonOffset &&
            fi distFromRight <= maximizeButtonOffset + buttonSize = focus mainw >> sendMessage (maximizeRestore mainw) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons = def {
                                windowTitleIcons = [ (menuButton, CenterLeft 3),
                                                     (closeButton, CenterRight 3),
                                                     (maxiButton, CenterRight 18),
                                                     (miniButton, CenterRight 33) ]
                               }

imageButtonDeco :: (Eq a, Shrinker s) => s -> Theme
                   -> l a -> ModifiedLayout (Decoration ImageButtonDecoration s) l a
imageButtonDeco s c = decoration s c $ NFD True

newtype ImageButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageButtonDecoration a where
    describeDeco _ = "ImageButtonDeco"
    decorationCatchClicksHook _ = imageTitleBarButtonHandler
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()
