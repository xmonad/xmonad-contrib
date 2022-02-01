{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.VoidBorders
-- Description :  Set borders to 0 for all windows in the workspace.
-- Copyright   :  Wilson Sales <spoonm@spoonm.org>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <spoonm@spoonm.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Modifies a layout to set borders to 0 for all windows in the workspace.
--
-- Unlike "XMonad.Layout.NoBorders", the 'voidBorders' modifier will not
-- restore the window border if the windows are moved to a different workspace
-- or the layout is changed. There is, however, a companion 'normalBorders'
-- modifier which explicitly restores the border.
--
-- This modifier's primary use is to eliminate the "border flash" you get
-- while switching workspaces with the "XMonad.Layout.NoBorders" modifier.
--
-----------------------------------------------------------------------------

module XMonad.Layout.VoidBorders ( -- * Usage
                                   -- $usage
                                   voidBorders
                                 , normalBorders
                                 ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet (integrate)

-- $usage
-- You can use this module with the following in your ~\/.xmonad/xmonad.hs
-- file:
--
-- > import XMonad.Layout.VoidBorders
--
-- and modify the layouts to call 'voidBorders' on the layouts you want to
-- remove borders from windows, and 'normalBorders' on the layouts you want
-- to keep borders for:
--
-- > layoutHook = ... ||| voidBorders Full ||| normalBorders Tall ...
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data VoidBorders a = VoidBorders deriving (Read, Show)

instance LayoutModifier VoidBorders Window where
  modifierDescription = const "VoidBorders"

  redoLayout VoidBorders _ Nothing wrs = return (wrs, Nothing)
  redoLayout VoidBorders _ (Just s) wrs = do
    mapM_ setZeroBorder $ integrate s
    return (wrs, Nothing)

voidBorders :: l Window -> ModifiedLayout VoidBorders l Window
voidBorders = ModifiedLayout VoidBorders

data NormalBorders a = NormalBorders deriving (Read, Show)

instance LayoutModifier NormalBorders Window where
  modifierDescription = const "NormalBorders"

  redoLayout NormalBorders _ Nothing wrs = return (wrs, Nothing)
  redoLayout NormalBorders _ (Just s) wrs = do
    mapM_ resetBorders $ integrate s
    return (wrs, Nothing)

normalBorders :: l Window -> ModifiedLayout NormalBorders l Window
normalBorders = ModifiedLayout NormalBorders

-- | Sets border width to 0 for every window from the specified layout.
setZeroBorder :: Window -> X ()
setZeroBorder w = setBorders w 0

-- | Resets the border to the value read from the current configuration.
resetBorders :: Window -> X ()
resetBorders w = asks (borderWidth . config) >>= setBorders w

setBorders :: Window -> Dimension -> X ()
setBorders w bw = withDisplay $ \d -> io $ setWindowBorderWidth d w bw
