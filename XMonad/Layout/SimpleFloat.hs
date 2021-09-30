{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SimpleFloat
-- Description :  A basic floating layout.
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A basic floating layout.
-----------------------------------------------------------------------------

module XMonad.Layout.SimpleFloat
    ( -- * Usage:
      -- $usage
      simpleFloat
    , simpleFloat'
    , SimpleDecoration (..)
    , SimpleFloat (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Actions.MouseResize
import XMonad.Layout.Decoration
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.WindowArranger

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.SimpleFloat
--
-- Then edit your @layoutHook@ by adding the SimpleFloat layout:
--
-- > myLayout = simpleFloat ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A simple floating layout where every window is placed according
-- to the window's initial attributes.
--
-- This version is decorated with the 'SimpleDecoration' style.
simpleFloat :: Eq a => ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
               (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
simpleFloat = decoration shrinkText def (Simple False) (mouseResize $ windowArrangeAll $ SF 20)

-- | Same as 'simpleFloat', but with the possibility of setting a
-- custom shrinker and a custom theme.
simpleFloat' :: (Eq a, Shrinker s) => s -> Theme ->
               ModifiedLayout (Decoration SimpleDecoration s)
               (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
simpleFloat' s c = decoration s c (Simple False) (mouseResize $ windowArrangeAll $ SF (decoHeight c))

newtype SimpleFloat a = SF Dimension deriving (Show, Read)
instance LayoutClass SimpleFloat Window where
    description _ = "Float"
    doLayout (SF i) sc (S.Stack w l r) = do
        wrs <- mapM (getSize i sc) (w : reverse l ++ r)
        return (wrs, Nothing)

getSize :: Dimension -> Rectangle -> Window -> X (Window,Rectangle)
getSize i (Rectangle rx ry _ _) w = do
  d  <- asks display
  bw <- asks (borderWidth . config)
  wa <- io $ getWindowAttributes d w
  let ny = ry + fi i
      x  =  max rx $ fi $ wa_x wa
      y  =  max ny $ fi $ wa_y wa
      wh = fi (wa_width  wa) + (bw * 2)
      ht = fi (wa_height wa) + (bw * 2)
  return (w, Rectangle x y wh ht)
