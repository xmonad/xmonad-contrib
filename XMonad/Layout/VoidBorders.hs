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
-- Unlike XMonad.Layout.NoBorders, this modifier will not restore the window
-- border if the windows are moved to a different workspace or the layout is
-- changed.
--
-- This modifier's primary use is to eliminate the "border flash" you get
-- while switching workspaces with the `noBorders` modifier. It won't return
-- the borders to their original width, however.
--
-----------------------------------------------------------------------------

module XMonad.Layout.VoidBorders ( -- * Usage
                                   -- $usage
                                   voidBorders
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
-- remove borders from windows:
--
-- > layoutHook = ... ||| voidBorders Full ||| ...
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data VoidBorders a = VoidBorders deriving (Read, Show)

voidBorders :: l Window -> ModifiedLayout VoidBorders l Window
voidBorders = ModifiedLayout VoidBorders

instance LayoutModifier VoidBorders Window where
  modifierDescription = const "VoidBorders"

  redoLayout VoidBorders _ Nothing wrs = return (wrs, Nothing)
  redoLayout VoidBorders _ (Just s) wrs = do
    mapM_ setZeroBorder $ integrate s
    return (wrs, Nothing)

-- | Sets border width to 0 for every window from the specified layout.
setZeroBorder :: Window -> X ()
setZeroBorder w = withDisplay $ \d -> io $ setWindowBorderWidth d w 0
