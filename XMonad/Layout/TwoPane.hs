{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TwoPane
-- Description :  A layout that splits the screen horizontally and shows two windows.
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that splits the screen horizontally and shows two windows.  The
-- left window is always the master window, and the right is either the
-- currently focused window or the second window in layout order.
--
-----------------------------------------------------------------------------

module XMonad.Layout.TwoPane (
                              -- * Usage
                              -- $usage
                              TwoPane (..)
                             ) where

import XMonad hiding (focus)
import XMonad.StackSet ( focus, up, down)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TwoPane
--
-- Then edit your @layoutHook@ by adding the TwoPane layout:
--
-- > myLayout = TwoPane (3/100) (1/2)  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data TwoPane a =
    TwoPane Rational Rational
    deriving ( Show, Read )

instance LayoutClass TwoPane a where
    doLayout (TwoPane _ split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (up st) of
                              (master:_) -> [(master,left),(focus st,right)]
                              [] -> case down st of
                                      (next:_) -> [(focus st,left),(next,right)]
                                      [] -> [(focus st, rect)]
              where (left, right) = splitHorizontallyBy split rect

    handleMessage (TwoPane delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (TwoPane delta (split - delta))
                   Just Expand -> Just (TwoPane delta (split + delta))
                   _           -> Nothing

    description _ = "TwoPane"
