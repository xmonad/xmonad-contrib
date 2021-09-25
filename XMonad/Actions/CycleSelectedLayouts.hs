-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleSelectedLayouts
-- Description :  Cycle through the given subset of layouts.
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module allows to cycle through the given subset of layouts.
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleSelectedLayouts (
    -- * Usage
    -- $usage
    cycleThroughLayouts) where

import XMonad
import XMonad.Prelude (elemIndex, fromMaybe)
import qualified XMonad.StackSet as S

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Actions.CycleSelectedLayouts
--
-- >   , ((modm,  xK_t ),   cycleThroughLayouts ["Tall", "Mirror Tall"])

cycleToNext :: (Eq a) => [a] -> a -> Maybe a
cycleToNext lst a = do
    -- not beautiful but simple and readable
    ind <- elemIndex a lst
    return $ lst !! if ind == length lst - 1 then 0 else ind+1

-- | If the current layout is in the list, cycle to the next layout. Otherwise,
--   apply the first layout from list.
cycleThroughLayouts :: [String] -> X ()
cycleThroughLayouts lst = do
    winset <- gets windowset
    let ld = description . S.layout . S.workspace . S.current $ winset
    let newld = fromMaybe (head lst) (cycleToNext lst ld)
    sendMessage $ JumpToLayout newld
