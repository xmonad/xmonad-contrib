-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.CopyWindow
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a binding to duplicate a window on multiple workspaces,
-- providing dwm-like tagging functionality.
--
-----------------------------------------------------------------------------

module XMonadContrib.CopyWindow (
                                 -- * Usage
                                 -- $usage
                                 copy, kill1
                                ) where

import Prelude hiding ( filter )
import Control.Monad.State ( gets )
import qualified Data.List as L
import XMonad
import Operations ( windows, kill )
import StackSet

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.CopyWindow
--
-- > -- mod-[1..9] @@ Switch to workspace N
-- > -- mod-shift-[1..9] @@ Move client to workspace N
-- > -- mod-control-shift-[1..9] @@ Copy client to workspace N
-- > [((m .|. modMask, k), f i)
-- >     | (i, k) <- zip workspaces [xK_1 ..]
-- >     , (f, m) <- [(view, 0), (shift, shiftMask), (copy, shiftMask .|. controlMask)]]
--
-- you may also wish to redefine the binding to kill a window so it only
-- removes it from the current workspace, if it's present elsewhere:
--
-- >  , ((modMask .|. shiftMask, xK_c     ), kill1) -- @@ Close the focused window

-- %import XMonadContrib.CopyWindow
-- %keybind -- comment out default close window binding above if you uncomment this:
-- %keybind , ((modMask .|. shiftMask, xK_c     ), kill1) -- @@ Close the focused window
-- %keybindlist ++
-- %keybindlist -- mod-[1..9] @@ Switch to workspace N
-- %keybindlist -- mod-shift-[1..9] @@ Move client to workspace N
-- %keybindlist -- mod-control-shift-[1..9] @@ Copy client to workspace N
-- %keybindlist [((m .|. modMask, k), f i)
-- %keybindlist     | (i, k) <- zip workspaces [xK_1 ..]
-- %keybindlist     , (f, m) <- [(view, 0), (shift, shiftMask), (copy, shiftMask .|. controlMask)]]

-- | copy. Copy a window to a new workspace.
copy :: WorkspaceId -> WindowSet -> WindowSet
copy n = copy'
    where copy' s = if n `tagMember` s && n /= tag (workspace (current s))
                    then maybe s (go s) (peek s)
                    else s
          go s w = view (tag (workspace (current s))) $ insertUp' w $ view n s
          insertUp' a s = modify (Just $ Stack a [] [])
                          (\(Stack t l r) -> Just $ Stack a (L.delete a l) (L.delete a (t:r))) s

-- | Remove the focussed window from this workspace.  If it's present in no
-- other workspace, then kill it instead. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
--
kill1 :: X ()
kill1 = do ss <- gets windowset
           whenJust (peek ss) $ \w -> if member w $ delete'' w ss
                                      then windows $ delete'' w
                                      else kill
    where delete'' w = sink w . modify Nothing (filter (/= w))
