-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.WithAll
-- Description  : Perform a given action on all or certain groups of windows.
-- License      : BSD3-style (see LICENSE)
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides functions for performing a given action on all or certain
-- groups of windows on the current workspace.
-----------------------------------------------------------------------------

module XMonad.Actions.WithAll (
    -- * Usage
    -- $usage
    sinkAll, withAll,
    withAll', killAll,
    killOthers) where

import XMonad.Prelude hiding (foldr)

import XMonad
import XMonad.StackSet

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.WithAll
--
-- then add a keybinding; for example:
--
--     , ((modm .|. shiftMask, xK_t), sinkAll)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Un-float all floating windows on the current workspace.
sinkAll :: X ()
sinkAll = withAll' sink

-- | Apply a function to all windows on the current workspace.
withAll' :: (Window -> WindowSet -> WindowSet) -> X ()
withAll' f = windows $ \ws -> let all' = integrate' . stack . workspace . current $ ws
                              in foldr f ws all'

-- | Execute an 'X' action for each window on the current workspace.
withAll :: (Window -> X ()) -> X()
withAll f = withWindowSet $ \ws -> let all' = integrate' . stack . workspace . current $ ws
                                   in forM_ all' f

-- | Kill all the windows on the current workspace.
killAll :: X()
killAll = withAll killWindow

-- | Kill all the unfocused windows on the current workspace.
killOthers :: X ()
killOthers = withUnfocused killWindow
