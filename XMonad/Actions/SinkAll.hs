-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.SinkAll
-- Description  : (DEPRECATED) Push floating windows back into tiling.
-- License      : BSD3-style (see LICENSE)
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides a simple binding that pushes all floating windows on the
-- current workspace back into tiling.  Note that the functionality of
-- this module has been folded into the more general
-- "XMonad.Actions.WithAll"; this module simply re-exports the
-- 'sinkAll' function for backwards compatibility.
-----------------------------------------------------------------------------

module XMonad.Actions.SinkAll (
    -- * Usage
    -- $usage

    sinkAll) where

import XMonad.Actions.WithAll (sinkAll)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.SinkAll
--
-- then add a keybinding; for example:
--
-- >   , ((modm .|. shiftMask, xK_t), sinkAll)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
