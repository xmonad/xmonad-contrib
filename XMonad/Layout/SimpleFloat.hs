{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SimpleFloat
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A very simple layout. The simplest, afaik.
-----------------------------------------------------------------------------

module XMonad.Layout.SimpleFloat
    ( -- * Usage:
      -- $usage
      simpleFloat
    , simpleFloat'
    , SimpleDecoration (..)
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import XMonad
import qualified XMonad.StackSet as S
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
-- > myLayouts = simpleFloat ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | FIXME
simpleFloat :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
	       (ModifiedLayout WindowArranger SimpleFloat) a
simpleFloat = decoration shrinkText defaultTheme (Simple False) (windowArrangeAll $ SF 20)

-- | FIXME
simpleFloat' :: Shrinker s => s -> Theme -> 
               ModifiedLayout (Decoration SimpleDecoration s)
	      (ModifiedLayout WindowArranger SimpleFloat) a
simpleFloat' s c = decoration s c (Simple False) (windowArrangeAll $ SF (decoHeight c))

data SimpleFloat a = SF Dimension deriving (Show, Read)
instance LayoutClass SimpleFloat Window where
    doLayout (SF i) sc (S.Stack w l r) = do wrs <- mapM (getSize i sc) (w : reverse l ++ r)
                                            return (wrs, Nothing)
    description _ = "Float"

getSize :: Dimension -> Rectangle -> Window -> X (Window,Rectangle)
getSize i (Rectangle rx ry _ _) w = do
  d  <- asks display
  bw <- asks (borderWidth . config)
  wa <- io $ getWindowAttributes d w
  let ny = ry + fi i
      x  =  max rx $ fi $ wa_x wa
      y  =  max ny $ fi $ wa_y wa
      wh = (fi $ wa_width  wa) + (bw * 2)
      ht = (fi $ wa_height wa) + (bw * 2)
  return (w, Rectangle x y wh ht)
