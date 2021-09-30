{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SimplestFloat
-- Description :  Like "XMonad.Layout.SimpleFloat" but without the decoration.
-- Copyright   :  (c) 2008 Jussi Mäki
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  joamaki@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A basic floating layout like SimpleFloat but without the decoration.
-----------------------------------------------------------------------------

module XMonad.Layout.SimplestFloat
    ( -- * Usage:
      -- $usage
      simplestFloat
    , SimplestFloat
    ) where

import XMonad.Prelude (fi)
import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.WindowArranger
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.SimplestFloat
--
-- Then edit your @layoutHook@ by adding the SimplestFloat layout:
--
-- > myLayout = simplestFloat ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A simple floating layout where every window is placed according
-- to the window's initial attributes.
simplestFloat :: Eq a => (ModifiedLayout WindowArranger SimplestFloat) a
simplestFloat = windowArrangeAll SF

data SimplestFloat a = SF deriving (Show, Read)
instance LayoutClass SimplestFloat Window where
    doLayout SF sc (S.Stack w l r) =  (, Nothing)
                                  <$> mapM (getSize sc) (w : reverse l ++ r)
    description _ = "SimplestFloat"

getSize :: Rectangle -> Window -> X (Window,Rectangle)
getSize (Rectangle rx ry _ _) w = do
  d  <- asks display
  bw <- asks (borderWidth . config)
  wa <- io $ getWindowAttributes d w
  let x  =  max rx $ fi $ wa_x wa
      y  =  max ry $ fi $ wa_y wa
      wh = fi (wa_width  wa) + (bw * 2)
      ht = fi (wa_height wa) + (bw * 2)
  return (w, Rectangle x y wh ht)
