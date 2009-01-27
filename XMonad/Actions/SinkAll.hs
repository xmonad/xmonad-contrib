-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.SinkAll
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
