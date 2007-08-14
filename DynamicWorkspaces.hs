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

import XMonad ( X )
import Operations ( windows )
import StackSet ( tagMember, StackSet(..), Screen(..), Workspace(..) )

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.DynamicWorkspaces
--
-- >   , ((modMask .|. shiftMask, xK_Up), addWorkspace)
-- >   , ((modMask .|. shiftMask, xK_Down), removeWorkspace)

addWorkspace :: X ()
addWorkspace = windows addWorkspace'

removeWorkspace :: X ()
removeWorkspace = windows removeWorkspace'

addWorkspace' :: (Enum i, Num i) => StackSet i a sid sd -> StackSet i a sid sd
addWorkspace' s@(StackSet { current = scr@(Screen { workspace = w })
                          , hidden = ws })
    = s { current = scr { workspace = Workspace newtag Nothing }
        , hidden = w:ws }
    where (newtag:_) = filter (not . (`tagMember` s)) [0..]

removeWorkspace' :: StackSet i a sid sd -> StackSet i a sid sd
removeWorkspace' s@(StackSet { current = scr@(Screen { workspace = Workspace { stack = Nothing } })
                             , hidden = (w:ws) })
                    = s { current = scr { workspace = w }
                        , hidden = ws }
removeWorkspace' s = s
