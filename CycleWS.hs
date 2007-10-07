-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.CycleWS
-- Copyright   :  (c) Joachim Breitner <mail@joachim-breitner.de>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle forward or backward through the list
-- of workspaces, and to move windows there.
--
-----------------------------------------------------------------------------

module XMonadContrib.CycleWS (
                              -- * Usage
                              -- $usage
                              nextWS,
                              prevWS,
			      shiftToNext,
			      shiftToPrev,
                             ) where

import Control.Monad.State ( gets )
import Data.List ( sortBy, findIndex )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )

import XMonad
import StackSet hiding (filter, findIndex)
import Operations
import {-# SOURCE #-} qualified Config (workspaces)

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.NextWorkspace
--
-- >   , ((modMask,               xK_Right), nextWS)
-- >   , ((modMask,               xK_Left),  prevWWS)
-- >   , ((modMask .|. shiftMask, xK_Right), shiftToNext)
-- >   , ((modMask .|. shiftMask, xK_Left),  shiftToPrev)
--
-- If you want to follow the moved window, you can use both actions:
--
-- >   , ((modMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
-- >   , ((modMask .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
--

-- %import XMonadContrib.NextWorkspace
-- %keybind , ((modMask,               xK_Right), nextWS)
-- %keybind , ((modMask,               xK_Left),  prevWWS)
-- %keybind , ((modMask .|. shiftMask, xK_Right), shiftToNext)
-- %keybind , ((modMask .|. shiftMask, xK_Left),  shiftToPrev)


-- ---------------------
-- |
-- Switch to next workspace
nextWS :: X()
nextWS = switchWorkspace (1)

-- ---------------------
-- |
-- Switch to previous workspace
prevWS :: X()
prevWS = switchWorkspace (-1)

-- |
-- Move focused window to next workspace
shiftToNext :: X()
shiftToNext = shiftBy (1)

-- |
-- Move focused window to previous workspace
shiftToPrev :: X ()
shiftToPrev = shiftBy (-1)

switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . greedyView

shiftBy :: Int -> X ()
shiftBy d = wsBy d >>= windows . shift

wsBy :: Int -> X (WorkspaceId)
wsBy d = do
    ws <- gets windowset
    let orderedWs = sortBy (comparing wsIndex) (workspaces ws)
    let now = fromMaybe 0 $ findWsIndex (workspace (current ws)) orderedWs
    let next = orderedWs !! ((now + d) `mod` length orderedWs)
    return $ tag next


wsIndex :: WindowSpace -> Maybe Int
wsIndex ws = findIndex (==(tag ws)) Config.workspaces

findWsIndex :: WindowSpace -> [WindowSpace] -> Maybe Int
findWsIndex ws wss = findIndex ((== tag ws) . tag) wss
