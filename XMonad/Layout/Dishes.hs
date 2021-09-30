{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Dishes
-- Description :  A layout that stacks extra windows underneath the master windows.
-- Copyright   :  (c) Jeremy Apthorp
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jeremy Apthorp <nornagon@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- Dishes is a layout that stacks extra windows underneath the master
-- windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Dishes (
                              -- * Usage
                              -- $usage
                              Dishes (..)
                            ) where

import XMonad
import XMonad.StackSet (integrate)
import XMonad.Prelude (ap)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Dishes
--
-- Then edit your @layoutHook@ by adding the Dishes layout:
--
-- > myLayout = Dishes 2 (1/6) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data Dishes a = Dishes Int Rational deriving (Show, Read)
instance LayoutClass Dishes a where
    doLayout (Dishes nmaster h) r =
        return . (, Nothing) .
        ap zip (dishes h r nmaster . length) . integrate
    pureMessage (Dishes nmaster h) m = fmap incmastern (fromMessage m)
        where incmastern (IncMasterN d) = Dishes (max 0 (nmaster+d)) h

dishes :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
dishes h s nmaster n = if n <= nmaster
                        then splitHorizontally n s
                        else ws
 where
    (m,rest) = splitVerticallyBy (1 - fromIntegral (n - nmaster) * h) s
    ws = splitHorizontally nmaster m ++ splitVertically (n - nmaster) rest
