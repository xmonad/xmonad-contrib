{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CopyWindow
-- Copyright   :  (c) David Roundy <droundy@darcs.net>, Ivan Veselov <veselov@gmail.com>
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

module XMonad.Actions.CopyWindow (
                                 -- * Usage
                                 -- $usage
                                 copy, copyToAll, copyWindow, killAllOtherCopies, kill1
                                ) where

import Prelude hiding (filter)
import qualified Data.List as L
import XMonad hiding (modify, workspaces)
import XMonad.StackSet

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.CopyWindow
--
-- Then add something like this to your keybindings:
--
-- > -- mod-[1..9] @@ Switch to workspace N
-- > -- mod-shift-[1..9] @@ Move client to workspace N
-- > -- mod-control-shift-[1..9] @@ Copy client to workspace N
-- > [((m .|. modMask x, k), windows $ f i)
-- >     | (i, k) <- zip (workspaces x) [xK_1 ..]
-- >     , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
--
-- To use the above key bindings you need also to import
-- "XMonad.StackSet":
--
-- > import qualified XMonad.StackSet as W
--
-- You may also wish to redefine the binding to kill a window so it only
-- removes it from the current workspace, if it's present elsewhere:
--
-- >  , ((modMask x .|. shiftMask, xK_c     ), kill1) -- @@ Close the focused window
--
-- Another possibility which this extension provides is 'making window
-- always visible' (i.e. always on current workspace), similar to corresponding
-- metacity functionality. This behaviour is emulated through copying given
-- window to all the workspaces and then removing it when it's unneeded on
-- all workspaces any more.
--
-- Here is the example of keybindings which provide these actions:
--
-- >  , ((modMask x, xK_v )", windows copyToAll) -- @@ Make focused window always visible
-- >  , ((modMask x .|. shiftMask, xK_v ),  killAllOtherCopies) -- @@ Toggle window state back
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | copy. Copy the focussed window to a new workspace.
copy :: (Eq s, Eq i, Eq a) => i -> StackSet i l a s sd -> StackSet i l a s sd
copy n s | Just w <- peek s = copyWindow w n s
         | otherwise = s

-- | copyToAll. Copy the focused window to all of workspaces.
copyToAll :: (Eq s, Eq i, Eq a) => StackSet i l a s sd -> StackSet i l a s sd
copyToAll s = foldr copy s $ map tag (workspaces s)

-- | copyWindow.  Copy a window to a new workspace
copyWindow :: (Eq a, Eq i, Eq s) => a -> i -> StackSet i l a s sd -> StackSet i l a s sd
copyWindow w n = copy'
    where copy' s = if n `tagMember` s
                    then view (tag (workspace (current s))) $ insertUp' w $ view n s
                    else s
          insertUp' a s = modify (Just $ Stack a [] [])
                          (\(Stack t l r) -> if a `elem` t:l++r
                                             then Just $ Stack t l r
                                             else Just $ Stack a (L.delete a l) (L.delete a (t:r))) s


-- | Remove the focused window from this workspace.  If it's present in no
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
    where delete'' w = modify Nothing (filter (/= w))

-- | Kill all other copies of focused window (if they're present)
-- 'All other' means here 'copies, which are not on current workspace'
--
-- Consider calling this function after copyToAll
--
killAllOtherCopies :: X ()
killAllOtherCopies = do ss <- gets windowset
                        whenJust (peek ss) $ \w -> windows $
                                                   view (tag (workspace (current ss))) .
                                                   delFromAllButCurrent w
    where
      delFromAllButCurrent w ss = foldr ($) ss $
                                  map (delWinFromWorkspace w . tag) $
                                  hidden ss ++ map workspace (visible ss)
      delWinFromWorkspace w wid ss = modify Nothing (filter (/= w)) $ view wid ss
