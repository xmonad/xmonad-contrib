{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Dishes
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

module XMonadContrib.Dishes (
                              -- * Usage
															-- $usage
                              Dishes (..)
                            ) where

import Data.List
import XMonad
import Operations
import StackSet (integrate)
import Control.Monad (ap)
import Graphics.X11.Xlib

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.Dishes
--
-- and add the following line to your 'defaultLayouts'
--
-- > , Layout $ Dishes 2 (1%6)

-- %import XMonadContrib.Dishes
-- %layout , Layout $ Dishes 2 (1%6)

data Dishes a = Dishes Int Rational deriving (Show, Read)
instance LayoutClass Dishes a where
	doLayout (Dishes nmaster h) r =
		return . (\x->(x,Nothing)) .
		ap zip (dishes h r nmaster . length) . integrate
	pureMessage (Dishes nmaster h) m = fmap incmastern (fromMessage m)
		where incmastern (IncMasterN d) = Dishes (max 0 (nmaster+d)) h

dishes :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
dishes h s nmaster n = if n <= nmaster
		then splitHorizontally n s
		else ws
	where
		(m,rest) = splitVerticallyBy (1 - (fromIntegral $ n - nmaster) * h) s
		ws = splitHorizontally nmaster m ++ splitVertically (n - nmaster) rest
