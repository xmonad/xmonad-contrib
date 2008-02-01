-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RotView
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle through non-empty workspaces.
--
-----------------------------------------------------------------------------

module XMonad.Actions.RotView (
                              -- * Usage
                              -- $usage
                              rotView
                             ) where

import Data.List ( sortBy, find )
import Data.Maybe ( isJust )
import Data.Ord ( comparing )

import XMonad
import XMonad.StackSet hiding (filter)

-- $usage
--
-- NOTE: This module is deprecated; see "XMonad.Actions.CycleWS".
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.RotView
--
-- Then add appropriate key bindings, such as:
--
-- >   , ((modMask x .|. shiftMask, xK_Right), rotView True)
-- >   , ((modMask x .|. shiftMask, xK_Left), rotView False)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Cycle through non-empty workspaces.  True --> cycle in the forward
--   direction.  Note that workspaces cycle in order by tag, so if your
--   workspaces are not in tag-order, the cycling might seem wonky.
rotView :: Bool -> X ()
rotView forward = do
    ws <- gets windowset
    let currentTag = tag . workspace . current $ ws
        sortWs     = sortBy (comparing tag)
        isNotEmpty = isJust . stack
        sorted     = sortWs (hidden ws)
        pivoted    = let (a,b) = span ((< currentTag) . tag) sorted in b ++ a
        pivoted'   | forward   = pivoted
                   | otherwise = reverse pivoted
        nextws     = find isNotEmpty pivoted'
    whenJust nextws (windows . view . tag)
