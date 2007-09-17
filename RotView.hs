-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.RotView
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle through non-empty workspaces.
--
-----------------------------------------------------------------------------

module XMonadContrib.RotView (
                              -- * Usage
                              -- $usage
                              rotView
                             ) where

import Control.Monad.State ( gets )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe, isJust )
import Data.Ord ( comparing )

import XMonad
import StackSet hiding (filter)
import Operations

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.RotView
--
-- >   , ((modMask .|. shiftMask, xK_Right), rotView True)
-- >   , ((modMask .|. shiftMask, xK_Left), rotView False)

-- %import XMonadContrib.RotView
-- %keybind , ((modMask .|. shiftMask, xK_Right), rotView True)
-- %keybind , ((modMask .|. shiftMask, xK_Left), rotView False)

rotView :: Bool -> X ()
rotView b = do
    ws <- gets windowset
    let m = tag . workspace . current $ ws
        sortWs = sortBy (comparing tag)
        pivoted = uncurry (flip (++)) . span ((< m) . tag) . sortWs . hidden $ ws
        nextws = listToMaybe . filter (isJust . stack) . (if b then id else reverse) $ pivoted
    whenJust nextws (windows . view . tag)
