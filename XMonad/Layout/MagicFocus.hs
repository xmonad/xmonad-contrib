{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.MagicFocus
-- Copyright    : (c) Peter De Wachter <pdewacht@gmail.com>
-- License      : BSD
--
-- Maintainer   : Peter De Wachter <pdewacht@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Automagically put the focused window in the master area.
-----------------------------------------------------------------------------

module XMonad.Layout.MagicFocus
    (-- * Usage
     -- $usage
     magicFocus,
     promoteWarp,
     promoteWarp',
     followOnlyIf,
     disableFollowOnWS
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier

import XMonad.Actions.UpdatePointer(updatePointer, PointerPosition(TowardsCentre))
import Data.Monoid(All(..))
import qualified Data.Map as M

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MagicFocus
--
-- Then edit your @layoutHook@ by adding the magicFocus layout
-- modifier:
--
-- > myLayouts = magicFocus (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts,
-- >                               handleEventHook = promoteWarp }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Create a new layout which automagically puts the focused window
--   in the master area.
magicFocus :: l a -> ModifiedLayout MagicFocus l a
magicFocus = ModifiedLayout MagicFocus

data MagicFocus a = MagicFocus deriving (Show, Read)

instance LayoutModifier MagicFocus Window where
  modifyLayout MagicFocus (W.Workspace i l s) r =
    withWindowSet $ \wset ->
      runLayout (W.Workspace i l (s >>= \st -> Just $ swap st (W.peek wset))) r

swap :: (Eq a) => W.Stack a -> Maybe a -> W.Stack a
swap (W.Stack f u d) focused
    | Just f == focused = W.Stack f [] (reverse u ++ d)
    | otherwise         = W.Stack f u d

-- | An eventHook that overrides the normal focusFollowsMouse. When the mouse
-- it moved to another window, that window is replaced as the master, and the
-- mouse is warped to inside the new master.
--
-- It prevents infinite loops when focusFollowsMouse is true (the default), and
-- MagicFocus is in use when changing focus with the mouse.
--
-- This eventHook does nothing when there are floating windows on the current
-- workspace.
promoteWarp :: Event -> X All
promoteWarp = promoteWarp' (TowardsCentre 0.15 0.15)

-- | promoteWarp' allows you to specify an arbitrary PointerPosition to apply
-- when the mouse enters another window.
promoteWarp' :: PointerPosition -> Event -> X All
promoteWarp' pos e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode   e == notifyNormal = do
        ws <- gets windowset
        let foc = W.peek ws
            st = W.integrate' . W.stack . W.workspace $ W.current ws
            wsFloats = M.filterWithKey (\k _ -> k `elem` st) $ W.floating ws
        if Just w /= foc && M.null wsFloats then do
            windows (W.swapMaster . W.focusWindow w)
            updatePointer pos
            return $ All False
          else return $ All True
promoteWarp' _ _ = return $ All True

-- | Another event hook to override the focusFollowsMouse and make the pointer
-- only follow if a given condition is satisfied. This could be used to disable
-- focusFollowsMouse only for given workspaces or layouts.
-- Beware that your focusFollowsMouse setting is ignored if you use this event hook.
followOnlyIf :: X Bool -> Event -> X All
followOnlyIf cond e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode e == notifyNormal
    = whenX cond (focus w) >> return (All False)
followOnlyIf _ _ = return $ All True

-- | Disables focusFollow on the given workspaces:
disableFollowOnWS :: [WorkspaceId] -> X Bool
disableFollowOnWS wses = (`notElem` wses) `fmap` gets (W.currentTag . windowset)
