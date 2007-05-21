-- greedyView is an alternative to standard workspace switching.  When a
-- workspace is already visible on another screen, greedyView swaps the
-- contents of that other screen with the current screen.

module XMonadContrib.GreedyView (greedyView) where

import StackSet as W
import XMonad
import Operations
import Data.List (find)

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
