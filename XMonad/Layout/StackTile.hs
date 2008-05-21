{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.StackTile
-- Copyright   :  (c) Rickard Gustafsson <acura@allyourbase.se>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Rickard Gustafsson <acura@allyourbase.se>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A stacking layout, like dishes but with the ability to resize master pane.
-- Moastly usefull on small screens.
--
-----------------------------------------------------------------------------

module XMonad.Layout.StackTile (
                                    -- * Usage
                                    -- $usage
                                    StackTile(..)
                                   ) where

import XMonad hiding (tile)
import qualified XMonad.StackSet as W
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.StackTile
--
-- Then edit your @layoutHook@ by adding the ResizableTile layout:
--
-- > myLayouts =  StackTile 1 (3/100) (1/2) ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
data StackTile a = StackTile !Int !Rational !Rational deriving (Show, Read)

instance LayoutClass StackTile a where
    pureLayout (StackTile nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = tile frac r nmaster (length ws)

    pureMessage (StackTile nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = StackTile nmaster delta (max 0 $ frac-delta)
            resize Expand             = StackTile nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = StackTile (max 0 (nmaster+d)) delta frac

    description _ = "StackTile"

tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
    then splitHorizontally n r
    else splitHorizontally nmaster r1 ++ splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitVerticallyBy f r
