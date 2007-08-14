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

import Control.Monad.State ( get, gets, modify )

import XMonad ( X, XState(..), Layout, trace )
import Operations ( windows )
import StackSet ( tagMember, StackSet(..), Screen(..), Workspace(..),
                  integrate, differentiate )
import Data.Map ( delete, insert )
import Graphics.X11.Xlib ( Window )

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.DynamicWorkspaces
--
-- >   , ((modMask .|. shiftMask, xK_Up), addWorkspace defaultLayouts)
-- >   , ((modMask .|. shiftMask, xK_Down), removeWorkspace)

addWorkspace :: [Layout Window] -> X ()
addWorkspace (l:ls) = do s <- gets windowset
                         let newtag:_ = filter (not . (`tagMember` s)) [0..]
                         modify $ \st -> st { layouts = insert newtag (l,ls) $ layouts st }
                         windows (addWorkspace' newtag)
addWorkspace [] = trace "bad layouts in XMonadContrib.DynamicWorkspaces.addWorkspace\n"

removeWorkspace :: X ()
removeWorkspace = do XState { windowset = s, layouts = fls } <- get
                     let w = tag $ workspace $ current s
                     modify $ \st -> st { layouts = delete w fls }
                     windows removeWorkspace'

addWorkspace' :: i -> StackSet i a sid sd -> StackSet i a sid sd
addWorkspace' newtag s@(StackSet { current = scr@(Screen { workspace = w })
                                 , hidden = ws })
    = s { current = scr { workspace = Workspace newtag Nothing }
        , hidden = w:ws }

removeWorkspace' :: StackSet i a sid sd -> StackSet i a sid sd
removeWorkspace' s@(StackSet { current = scr@(Screen { workspace = Workspace { stack = st } })
                             , hidden = (w:ws) })
 = s { current = scr { workspace = w { stack = meld st (stack w) } }
     , hidden = ws }
   where meld Nothing Nothing = Nothing
         meld x Nothing = x
         meld Nothing x = x
         meld (Just x) (Just y) = differentiate (integrate x ++ integrate y)
removeWorkspace' s = s
