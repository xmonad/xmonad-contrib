{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CopyWindow
-- Copyright   :  (c) David Roundy <droundy@darcs.net>, Ivan Veselov <veselov@gmail.com>, Lanny Ripple <lan3ny@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  ???
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
                                 copy, copyToAll, copyWindow, runOrCopy
                                 , killAllOtherCopies, kill1
                                ) where

import Prelude hiding (filter)
import Control.Monad
import qualified Data.List as L
import XMonad hiding (modify, workspaces)
import XMonad.Actions.WindowGo
import qualified XMonad.StackSet as W

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
-- Instead of copying a window from a workset to a workset maybe you don't
-- want to have to remember where you placed it.  For that consider:
--
-- >  , ((modMask x, xK_b    ), runOrCopy "firefox" (className =? "Firefox")) -- @@ run or copy firefox
--
-- Another possibility which this extension provides is 'making window
-- always visible' (i.e. always on current workspace), similar to corresponding
-- metacity functionality. This behaviour is emulated through copying given
-- window to all the workspaces and then removing it when it's unneeded on
-- all workspaces any more.
--
-- Here is the example of keybindings which provide these actions:
--
-- >  , ((modMask x, xK_v ), windows copyToAll) -- @@ Make focused window always visible
-- >  , ((modMask x .|. shiftMask, xK_v ),  killAllOtherCopies) -- @@ Toggle window state back
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | copy. Copy the focused window to a new workspace.
copy :: (Eq s, Eq i, Eq a) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copy n s | Just w <- W.peek s = copyWindow w n s
         | otherwise = s

-- | copyToAll. Copy the focused window to all of workspaces.
copyToAll :: (Eq s, Eq i, Eq a) => W.StackSet i l a s sd -> W.StackSet i l a s sd
copyToAll s = foldr copy s $ map W.tag (W.workspaces s)

-- | copyWindow.  Copy a window to a new workspace
copyWindow :: (Eq a, Eq i, Eq s) => a -> i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWindow w n = copy'
    where copy' s = if n `W.tagMember` s
                    then W.view (W.currentTag s) $ insertUp' w $ W.view n s
                    else s
          insertUp' a s = W.modify (Just $ W.Stack a [] [])
                          (\(W.Stack t l r) -> if a `elem` t:l++r
                                             then Just $ W.Stack t l r
                                             else Just $ W.Stack a (L.delete a l) (L.delete a (t:r))) s


-- | runOrCopy .  runOrCopy will run the provided shell command unless it can
--  find a specified window in which case it will copy the window to
--  the current workspace.  Similar to (i.e., stolen from) "XMonad.Actions.WindowGo".
runOrCopy :: String -> Query Bool -> X ()
runOrCopy = copyMaybe . spawn

-- | Copy a window if it exists, run the first argument otherwise
copyMaybe :: X () -> Query Bool -> X ()
copyMaybe f qry = ifWindow qry copyWin f
    where copyWin = ask >>= \w -> doF (\ws -> copyWindow w (W.currentTag ws) ws)

-- | Remove the focused window from this workspace.  If it's present in no
-- other workspace, then kill it instead. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
kill1 :: X ()
kill1 = do ss <- gets windowset
           whenJust (W.peek ss) $ \w -> if W.member w $ delete'' w ss
                                      then windows $ delete'' w
                                      else kill
    where delete'' w = W.modify Nothing (W.filter (/= w))

-- | Kill all other copies of focused window (if they're present)
-- 'All other' means here 'copies, which are not on current workspace'
--
-- TODO: Call this function after 'copyToAll'?
killAllOtherCopies :: X ()
killAllOtherCopies = do ss <- gets windowset
                        whenJust (W.peek ss) $ \w -> windows $
                                                   W.view (W.currentTag ss) .
                                                   delFromAllButCurrent w
    where
      delFromAllButCurrent w ss = foldr ($) ss $
                                  map (delWinFromWorkspace w . W.tag) $
                                  W.hidden ss ++ map W.workspace (W.visible ss)
      delWinFromWorkspace w wid = W.modify Nothing (W.filter (/= w)) . W.view wid
