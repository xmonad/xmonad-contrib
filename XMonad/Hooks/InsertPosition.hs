-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.InsertPosition
-- Copyright   :  (c) 2009 Adam Vogt
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Configure where new windows should be added and which window should be
-- focused.
--
-----------------------------------------------------------------------------

-- TODO: note about XMonad.Hooks.Focus

module XMonad.Hooks.InsertPosition (
    -- * Usage
    -- $usage
    insertPosition
    ,Focus(..), Position(..)
    ) where

import XMonad(ManageHook, MonadReader(ask))
import XMonad.Prelude (Endo (Endo), find, fromMaybe)
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.InsertPosition
-- > xmonad def { manageHook = insertPosition Master Newer <+> myManageHook }
--
-- You should you put the manageHooks that use 'doShift' to take effect
-- /before/ 'insertPosition', so that the window order will be consistent.
-- Because ManageHooks compose from right to left (like function composition
-- '.'), this means that 'insertPosition' should be the leftmost ManageHook.

data Position = Master | End | Above | Below
data Focus = Newer | Older

-- | insertPosition. A manage hook for placing new windows. XMonad's default is
-- the same as using: @insertPosition Above Newer@.
insertPosition :: Position -> Focus -> ManageHook
insertPosition pos foc = Endo . g <$> ask
  where
    g w = viewingWs w (updateFocus w . ins w . W.delete' w)
    ins w = (\f ws -> fromMaybe id (W.focusWindow <$> W.peek ws) $ f ws) $
        case pos of
            Master -> W.insertUp w . W.focusMaster
            End    -> insertDown w . W.modify' focusLast'
            Above  -> W.insertUp w
            Below  -> insertDown w
    updateFocus =
        case foc of
            Older -> const id
            Newer -> W.focusWindow

-- | Modify the StackSet when the workspace containing w is focused
viewingWs :: (Eq a, Eq s, Eq i, Show i) =>a-> (W.StackSet i l a s sd -> W.StackSet i l a s sd)-> W.StackSet i l a s sd-> W.StackSet i l a s sd
viewingWs w f = do
    i <- W.tag . W.workspace . W.current
    ws <- find (elem w . W.integrate' . W.stack) . W.workspaces
    maybe id (fmap (W.view i . f) . W.view . W.tag) ws

-- | 'insertDown' and 'focusLast' belong in XMonad.StackSet?
insertDown :: (Eq a) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
insertDown w = W.swapDown . W.insertUp w

focusLast' ::  W.Stack a -> W.Stack a
focusLast' st = let ws = W.integrate st
    in W.Stack (last ws) (tail $ reverse ws) []
