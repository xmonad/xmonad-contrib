{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.FindEmptyWorkspace
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- GreedyView is an alternative to standard workspace switching.  When a
-- workspace is already visible on another screen, GreedyView swaps the
-- contents of that other screen with the current screen.
--
-----------------------------------------------------------------------------

module XMonadContrib.GreedyView (
                                 -- * Usage
                                 -- $usage
                                 greedyView
                                ) where

import StackSet as W hiding (filter)
import XMonad
import Operations
import Data.List (find)

-- $usage
-- To use GreedyView as your default workspace switcher
--
-- Add this import:
--
-- >     import XMonadContrib.GreedyView
--
-- And replace the function call used to switch workspaces,
--
-- this:
--
-- > [((m .|. modMask, k), f i)
-- >     | (i, k) <- zip [0 .. fromIntegral workspaces - 1] [xK_1 ..]
-- >     , (f, m) <- [(view, 0), (shift, shiftMask)]]
--
-- becomes this :
--
-- > [((m .|. modMask, k), f i)
-- >     | (i, k) <- zip [0 .. fromIntegral workspaces - 1] [xK_1 ..]
-- >     , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]
--

greedyView :: WorkspaceId -> X ()
greedyView = windows . greedyView'

greedyView' :: WorkspaceId -> WindowSet -> WindowSet
greedyView' w ws
 | any wTag (hidden ws)                             = W.view w ws
 | (Just s) <- find (wTag . workspace) (visible ws) = ws { current = setScreen s (screen $ current ws)
                                                         , visible = setScreen (current ws) (screen s)
                                                                   : filter (not . wTag . workspace) (visible ws)
                                                         }
 | otherwise                                        = ws
 where
    setScreen s i = s { screen = i }
    wTag = (w == ) . tag
