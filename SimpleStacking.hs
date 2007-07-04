{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SimpleStacking
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module to be used to obtain a simple "memory" of stacking order.
--
-----------------------------------------------------------------------------

module XMonadContrib.SimpleStacking (
                                     -- * Usage
                                     -- $usage 
                                     simpleStacking
                                    ) where

import Control.Monad.State ( get )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )

import Data.List ( nub, lookup, delete )
import StackSet ( focus, tag, workspace, current, up, down )
import Graphics.X11.Xlib ( Window )

import XMonad
import XMonadContrib.LayoutHelpers

-- $usage
-- You can use this module for 
-- See, for instance, "XMonadContrib.Tabbed"

simpleStacking :: Layout Window -> Layout Window
simpleStacking = simpleStacking' []

simpleStacking' :: [Window] -> Layout Window -> Layout Window
simpleStacking' st = layoutModify dl idModMod
    where dl r s wrs = let m = map (\ (w,rr) -> (w,(w,rr))) wrs
                           wrs' = catMaybes $ map ((flip lookup) m) $
                                  nub (focus s : st ++ map fst wrs)
                           st' = focus s:filter (`elem` (up s++down s)) st
                       in return (wrs', Just (simpleStacking' st'))
