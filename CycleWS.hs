-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.CycleWS
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module to cycle between Workspaces
--
-----------------------------------------------------------------------------

module XMonadContrib.CycleWS (
                             -- * Usage
                             -- $usage
                             nextWS
                             , prevWS
                              ) where

import XMonad
import Operations
import qualified StackSet as W
import {-# SOURCE #-} Config (workspaces)
import Data.List

-- $usage
-- Import this module in Config.hs:
--
-- > import XMonadContrib.CycleWS
--
-- And add, in you key bindings:
--
-- >     , ((modMask              , xK_comma ), prevWS ) 
-- >     , ((modMask              , xK_period), nextWS ) 

nextWS, prevWS :: X ()
nextWS = withWindowSet $ \s -> view (workspaces !! (setWS s N))
prevWS = withWindowSet $ \s -> view (workspaces !! (setWS s P))

data Dir = P | N deriving Eq
setWS :: WindowSet -> Dir -> Int
setWS s d 
    | d == N && cur == (lw - 1) = 0
    | d == N                    = cur + 1
    | d == P && cur == 0        = lw - 1
    | otherwise                 = cur - 1
      where 
        cur = maybe 0 id $ elemIndex (W.tag (W.workspace ((W.current s)))) workspaces
        lw  = length workspaces
