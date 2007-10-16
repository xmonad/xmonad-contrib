-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DynamicWorkspaces
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to add and delete workspaces.  Note that you may only
-- delete a workspace that is already empty.
--
-----------------------------------------------------------------------------

module XMonadContrib.DynamicWorkspaces (
                                         -- * Usage
                                         -- $usage
                                         addWorkspace, removeWorkspace,
                                         selectWorkspace
                                       ) where

import Control.Monad.State ( gets )

import XMonad ( X, XState(..), Layout, WorkspaceId )
import Operations
import StackSet hiding (filter, modify, delete)
import Graphics.X11.Xlib ( Window )
import XMonadContrib.WorkspacePrompt
import XMonadContrib.XPrompt ( XPConfig )

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.DynamicWorkspaces
--
-- >   , ((modMask .|. shiftMask, xK_n), selectWorkspace defaultXPConfig layoutHook)
-- >   , ((modMask .|. shiftMask, xK_BackSpace), removeWorkspace)

allPossibleTags :: [WorkspaceId]
allPossibleTags = map (:"") ['0'..]

selectWorkspace :: XPConfig -> Layout Window -> X ()
selectWorkspace conf l = workspacePrompt conf $ \w ->
                         do s <- gets windowset
                            if tagMember w s
                                then windows $ greedyView w
                                else windows $ addWorkspace' w l

addWorkspace :: Layout Window -> X ()
addWorkspace l = do s <- gets windowset
                    let newtag:_ = filter (not . (`tagMember` s)) allPossibleTags
                    windows (addWorkspace' newtag l)

removeWorkspace :: X ()
removeWorkspace = do s <- gets windowset
                     case s of
                       StackSet { current = Screen { workspace = torem }
                                , hidden = (w:_) }
                           -> do windows $ view (tag w)
                                 windows (removeWorkspace' (tag torem))
                       _ -> return ()

addWorkspace' :: i -> l -> StackSet i l a sid sd -> StackSet i l a sid sd
addWorkspace' newtag l s@(StackSet { current = scr@(Screen { workspace = w })
                                   , hidden = ws })
    = s { current = scr { workspace = Workspace newtag l Nothing }
        , hidden = w:ws }

removeWorkspace' :: (Eq i) => i -> StackSet i l a sid sd -> StackSet i l a sid sd
removeWorkspace' torem s@(StackSet { current = scr@(Screen { workspace = wc })
                                   , hidden = (w:ws) })
    | tag w == torem = s { current = scr { workspace = wc { stack = meld (stack w) (stack wc) } }
                         , hidden = ws }
   where meld Nothing Nothing = Nothing
         meld x Nothing = x
         meld Nothing x = x
         meld (Just x) (Just y) = differentiate (integrate x ++ integrate y)
removeWorkspace' _ s = s
