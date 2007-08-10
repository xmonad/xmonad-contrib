-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ViewPrev
-- Copyright   :  (c) Nelson Elhage <nelhage@mit.edu>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Nelson Elhage <nelhage@mit.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that implements a command to switch to the previously
-- viewed workspace
--
-----------------------------------------------------------------------------

module XMonadContrib.ViewPrev (
                               viewPrev
                              ) where

import XMonad
import Operations 
import qualified StackSet as W

viewPrev' :: (Eq a, Eq s, Eq i) => W.StackSet i a s sd -> W.StackSet i a s sd
viewPrev' x = W.view (W.tag . head . W.hidden $ x) x

viewPrev :: X ()
viewPrev = windows viewPrev'
