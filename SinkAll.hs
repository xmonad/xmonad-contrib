-----------------------------------------------------------------------------
-- |
-- Module       : XmonadContrib.SinkAll
-- License      : BSD3-style (see LICENSE)
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides a simple binding that pushes all floating windows on the current
-- workspace back into tiling.
-----------------------------------------------------------------------------

module XMonadContrib.SinkAll (
    -- * Usage
    -- $usage
    sinkAll) where

import Operations
import XMonad
import StackSet

import Control.Monad.State
import Graphics.X11.Xlib

-- $usage
-- > import XMonadContrib.SinkAll
-- > keys = [ ((modMask .|. shiftMask, xK_t), sinkAll) ]

-- %import XMonadContrib.SinkAll
-- %keybind , ((modMask .|. shiftMask, xK_t), sinkAll)

sinkAll :: X ()
sinkAll = withAll sink

-- Apply a function to all windows on current workspace.
withAll :: (Window -> WindowSet -> WindowSet) -> X ()
withAll f = windows $ \ws -> let all = integrate' . stack . workspace . current $ ws
                             in foldr f ws all
