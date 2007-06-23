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
import StackSet hiding (sink)

import Control.Monad.State
import Graphics.X11.Xlib

-- $usage
-- > import XMonadContrib.SinkAll
-- > keys = [ ((modMask .|. shiftMask, xK_t), sinkAll) ]

sinkAll :: X ()
sinkAll = withAll sink

-- Apply a function to all windows on current workspace.
withAll :: (Window -> X a) -> X ()
withAll f = gets (integrate' . stack . workspace . current . windowset) >>=
    mapM_ f
