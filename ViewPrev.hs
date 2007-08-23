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

viewPrev :: X ()
viewPrev = windows viewPrev'
    where viewPrev' x = W.view (W.tag . head . W.hidden $ x) x
