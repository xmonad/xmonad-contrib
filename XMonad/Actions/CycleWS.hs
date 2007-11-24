-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleWS
-- Copyright   :  (c) Joachim Breitner <mail@joachim-breitner.de>,
--                    Nelson Elhage <nelhage@mit.edu> (`toggleWS' function)
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

module XMonad.Actions.CycleWS (
                              -- * Usage
                              -- $usage
                              nextWS,
                              prevWS,
                              shiftToNext,
                              shiftToPrev,
                              toggleWS,
                             ) where

import Control.Monad.Reader ( asks )
import Control.Monad.State ( gets )
import Data.List ( sortBy, findIndex )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )

import XMonad hiding (workspaces)
import qualified XMonad (workspaces)
import XMonad.StackSet hiding (filter)
import XMonad.Operations

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.CycleWS
--
-- >   , ((modMask x,               xK_Right), nextWS)
-- >   , ((modMask x,               xK_Left),  prevWS)
-- >   , ((modMask x .|. shiftMask, xK_Right), shiftToNext)
-- >   , ((modMask x .|. shiftMask, xK_Left),  shiftToPrev)
-- >   , ((modMask x,               xK_t),     toggleWS)
--
-- If you want to follow the moved window, you can use both actions:
--
-- >   , ((modMask x .|. shiftMask, xK_Right), shiftToNext >> nextWS)
-- >   , ((modMask x .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".


-- | Switch to next workspace
nextWS :: X ()
nextWS = switchWorkspace 1

-- | Switch to previous workspace
prevWS :: X ()
prevWS = switchWorkspace (-1)

-- | Move focused window to next workspace
shiftToNext :: X ()
shiftToNext = shiftBy 1

-- | Move focused window to previous workspace
shiftToPrev :: X ()
shiftToPrev = shiftBy (-1)

-- | Toggle to the workspace displayed previously
toggleWS :: X ()
toggleWS = windows $ view =<< tag . head . hidden

switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . greedyView

shiftBy :: Int -> X ()
shiftBy d = wsBy d >>= windows . shift

wsBy :: Int -> X (WorkspaceId)
wsBy d = do
    ws <- gets windowset
    spaces <- asks (XMonad.workspaces . config)
    let orderedWs = sortBy (comparing (wsIndex spaces)) (workspaces ws)
    let now = fromMaybe 0 $ findWsIndex (workspace (current ws)) orderedWs
    let next = orderedWs !! ((now + d) `mod` length orderedWs)
    return $ tag next

wsIndex :: [WorkspaceId] -> WindowSpace -> Maybe Int
wsIndex spaces ws = findIndex (== tag ws) spaces

findWsIndex :: WindowSpace -> [WindowSpace] -> Maybe Int
findWsIndex ws wss = findIndex ((== tag ws) . tag) wss
