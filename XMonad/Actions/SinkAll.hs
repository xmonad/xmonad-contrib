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

import XMonad
import XMonad.StackSet

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.SinkAll
--
-- then add a keybinding; for example:
--
--     , ((modMask x .|. shiftMask, xK_t), sinkAll)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Un-float all floating windows on the current workspace.
sinkAll :: X ()
sinkAll = withAll sink

-- | Apply a function to all windows on current workspace.
withAll :: (Window -> WindowSet -> WindowSet) -> X ()
withAll f = windows $ \ws -> let all' = integrate' . stack . workspace . current $ ws
                             in foldr f ws all'
