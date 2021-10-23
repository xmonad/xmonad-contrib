----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.PositionStoreHooks
-- Description :  Hooks for XMonad.Util.PositionStore.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- This module contains two hooks for the
-- PositionStore (see "XMonad.Util.PositionStore") - a ManageHook and
-- an EventHook.
--
-- The ManageHook can be used to fill the PositionStore with position and size
-- information about new windows. The advantage of using this hook is, that the
-- information is recorded independent of the currently active layout. So the
-- floating shape of the window can later be restored even if it was opened in a
-- tiled layout initially.
--
-- For windows, that do not request a particular position, a random position will
-- be assigned. This prevents windows from piling up exactly on top of each other.
--
-- The EventHook makes sure that windows are deleted from the PositionStore
-- when they are closed.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.PositionStoreHooks (
    -- * Usage
    -- $usage
    positionStoreManageHook,
    positionStoreEventHook
    ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Util.PositionStore
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration

import System.Random(randomRIO)
import qualified Data.Set as S

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.PositionStoreHooks
--
-- and adding 'positionStoreManageHook' to your 'ManageHook' as well
-- as 'positionStoreEventHook' to your event hooks. To be accurate
-- about window sizes, the module needs to know if any decoration is in effect.
-- This is specified with the first argument: Supply 'Nothing' for no decoration,
-- otherwise use 'Just def' or similar to inform the module about the
-- decoration theme used.
--
-- > myManageHook = positionStoreManageHook Nothing <+> manageHook def
-- > myHandleEventHook = positionStoreEventHook
-- >
-- > main = xmonad def { manageHook = myManageHook
-- >                   , handleEventHook = myHandleEventHook
-- >                   }
--

positionStoreManageHook :: Maybe Theme -> ManageHook
positionStoreManageHook mDecoTheme = ask >>= liftX . positionStoreInit mDecoTheme >> idHook

positionStoreInit :: Maybe Theme -> Window -> X ()
positionStoreInit mDecoTheme w  = withDisplay $ \d -> withWindowAttributes d w $ \wa -> do
        let decoH = maybe 0 decoHeight mDecoTheme   -- take decoration into account, which - in its current
                                                    -- form - makes windows smaller to make room for it
        ws <- gets windowset
        arbitraryOffsetX <- randomIntOffset
        arbitraryOffsetY <- randomIntOffset
        if (wa_x wa == 0) && (wa_y wa == 0)
            then do
                let sr@(Rectangle srX srY _ _) = screenRect . W.screenDetail . W.current $ ws
                modifyPosStore (\ps -> posStoreInsert ps w
                                        (Rectangle (srX + fi arbitraryOffsetX)
                                                   (srY + fi arbitraryOffsetY)
                                                    (fi $ wa_width wa)
                                                    (decoH + fi (wa_height wa))) sr )
            else do
                sc <- fromMaybe (W.current ws) <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
                let sr = screenRect . W.screenDetail $ sc
                sr' <- fmap ($ sr) (calcGap $ S.fromList [minBound .. maxBound]) -- take docks into account, accepting
                                                                                       -- a somewhat unfortunate inter-dependency
                                                                                       -- with 'XMonad.Hooks.ManageDocks'
                modifyPosStore (\ps -> posStoreInsert ps w
                                        (Rectangle (fi $ wa_x wa) (fi (wa_y wa) - fi decoH)
                                            (fi $ wa_width wa) (decoH + fi (wa_height wa))) sr' )
    where
        randomIntOffset :: X Int
        randomIntOffset = io $ randomRIO (42, 242)

positionStoreEventHook :: Event -> X All
positionStoreEventHook DestroyWindowEvent{ev_window = w, ev_event_type = et} = do
    when (et == destroyNotify) $
        modifyPosStore (`posStoreRemove` w)
    return (All True)
positionStoreEventHook _ = return (All True)
