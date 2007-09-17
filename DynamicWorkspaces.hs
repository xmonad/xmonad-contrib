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
                                         addWorkspace, removeWorkspace
                                       ) where

import Control.Monad.State ( gets, modify )

import XMonad ( X, XState(..), Layout, WorkspaceId, trace )
import Operations
import StackSet hiding (filter, modify, delete)
import Data.Map ( delete, insert )
import Graphics.X11.Xlib ( Window )

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.DynamicWorkspaces
--
-- >   , ((modMask .|. shiftMask, xK_Up), addWorkspace defaultLayouts)
-- >   , ((modMask .|. shiftMask, xK_Down), removeWorkspace)

allPossibleTags :: [WorkspaceId]
allPossibleTags = map (:"") ['0'..]

addWorkspace :: [Layout Window] -> X ()
addWorkspace (l:ls) = do s <- gets windowset
                         let newtag:_ = filter (not . (`tagMember` s)) allPossibleTags
                         modify $ \st -> st { layouts = insert newtag (l,ls) $ layouts st }
                         windows (addWorkspace' newtag)
addWorkspace [] = trace "bad layouts in XMonadContrib.DynamicWorkspaces.addWorkspace\n"

removeWorkspace :: X ()
removeWorkspace = do s <- gets windowset
                     case s of
                       StackSet { current = Screen { workspace = torem }
                                , hidden = (w:_) }
                           -> do windows $ view (tag w)
                                 modify $ \st -> st { layouts = delete (tag torem) $ layouts st }
                                 windows (removeWorkspace' (tag torem))
                       _ -> return ()

addWorkspace' :: i -> StackSet i a sid sd -> StackSet i a sid sd
addWorkspace' newtag s@(StackSet { current = scr@(Screen { workspace = w })
                                 , hidden = ws })
    = s { current = scr { workspace = Workspace newtag Nothing }
        , hidden = w:ws }

removeWorkspace' :: (Eq i) => i -> StackSet i a sid sd -> StackSet i a sid sd
removeWorkspace' torem s@(StackSet { current = scr@(Screen { workspace = wc })
                                   , hidden = (w:ws) })
    | tag w == torem = s { current = scr { workspace = wc { stack = meld (stack w) (stack wc) } }
                         , hidden = ws }
   where meld Nothing Nothing = Nothing
         meld x Nothing = x
         meld Nothing x = x
         meld (Just x) (Just y) = differentiate (integrate x ++ integrate y)
removeWorkspace' _ s = s
