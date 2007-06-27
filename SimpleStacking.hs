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
-- WARNING: This module is incompatible with Xinerama!
--
-----------------------------------------------------------------------------

module XMonadContrib.SimpleStacking (
                                     -- * Usage
                                     -- $usage 
                                     simpleStacking
                                    ) where

import Control.Monad.State ( modify )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )

import Data.List ( nub, lookup )
import StackSet ( focus, tag, workspace, current, integrate )
import Graphics.X11.Xlib ( Window )

import XMonad

-- $usage
-- You can use this module for 
-- See, for instance, "XMonadContrib.Tabbed"

simpleStacking :: Layout Window -> Layout Window
simpleStacking = simpleStacking' []

simpleStacking' :: [Window] -> Layout Window -> Layout Window
simpleStacking' st l = l { doLayout = dl
                         , modifyLayout = \m -> fmap (simpleStacking' st) `fmap` modifyLayout l m }
    where dl r s = do modify $ \ state ->
                          state { layouts = M.adjust
                                            (\(_,ss)->(simpleStacking'
                                                       (focus s:filter (`elem` integrate s) st) l,ss))
                                            (tag.workspace.current.windowset $ state)
                                            (layouts state) }
                      lo <- doLayout l r s
                      let m = map (\ (w,rr) -> (w,(w,rr))) lo
                      return $ catMaybes $ map ((flip lookup) m) $ nub (focus s : st ++ map fst lo)
