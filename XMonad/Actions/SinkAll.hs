-----------------------------------------------------------------------------
-- |
-- Module       : Xmonad.Actions.SinkAll
-- License      : BSD3-style (see LICENSE)
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides a simple binding that pushes all floating windows on the current
-- workspace back into tiling.
-----------------------------------------------------------------------------

module XMonad.Actions.SinkAll (
    -- * Usage
    -- $usage
    sinkAll) where

import XMonad.Operations
import XMonad
import XMonad.StackSet

import Graphics.X11.Xlib

-- $usage
-- > import XMonad.Actions.SinkAll
-- > keys = [ ((modMask .|. shiftMask, xK_t), sinkAll) ]

-- %import XMonad.Actions.SinkAll
-- %keybind , ((modMask .|. shiftMask, xK_t), sinkAll)

sinkAll :: X ()
sinkAll = withAll sink

-- Apply a function to all windows on current workspace.
withAll :: (Window -> WindowSet -> WindowSet) -> X ()
withAll f = windows $ \ws -> let all' = integrate' . stack . workspace . current $ ws
                             in foldr f ws all'
