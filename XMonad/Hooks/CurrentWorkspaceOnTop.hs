----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.CurrentWorkspaceOnTop
-- Description :  Ensure that windows on the current workspace are on top.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Ensures that the windows of the current workspace are always in front
-- of windows that are located on other visible screens. This becomes important
-- if you use decoration and drag windows from one screen to another. Using this
-- module, the dragged window will always be in front of other windows.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.CurrentWorkspaceOnTop (
    -- * Usage
    -- $usage
    currentWorkspaceOnTop
    ) where

import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prelude (unless, when)
import qualified Data.Map as M

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.CurrentWorkspaceOnTop
-- >
-- > main = xmonad $ def {
-- >    ...
-- >    logHook = currentWorkspaceOnTop
-- >    ...
-- >  }
--

newtype CWOTState = CWOTS String

instance ExtensionClass CWOTState where
  initialValue = CWOTS ""

currentWorkspaceOnTop :: X ()
currentWorkspaceOnTop = withDisplay $ \d -> do
    ws <- gets windowset
    (CWOTS lastTag) <- XS.get
    let curTag = S.tag . S.workspace . S.current $ ws
    when (curTag /= lastTag) $ do
        -- the following is more or less a reimplementation of what's happening in "XMonad.Operation"
        let s = S.current ws
            wsp = S.workspace s
            viewrect = screenRect $ S.screenDetail s
            tmpStack = S.stack wsp >>= S.filter (`M.notMember` S.floating ws)
        (rs, ml') <- runLayout wsp { S.stack = tmpStack } viewrect
        updateLayout curTag ml'
        let this = S.view curTag ws
            fltWins = filter (`M.member` S.floating ws) $ S.index this
            wins = fltWins ++ map fst rs  -- order: first all floating windows, then the order the layout returned
        -- end of reimplementation

        unless (null wins) $ do
            io $ raiseWindow d (head wins)  -- raise first window of current workspace to the very top,
            io $ restackWindows d wins      -- then use restackWindows to let all other windows from the workspace follow
        XS.put(CWOTS curTag)
