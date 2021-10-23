{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceByPos
-- Description :  Move new window to non-focused screen based on its requested geometry.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Useful in a dual-head setup: Looks at the requested geometry of
-- new windows and moves them to the workspace of the non-focused
-- screen if necessary.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.WorkspaceByPos (
    -- * Usage
    -- $usage
    workspaceByPos
    ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import Control.Monad.Except (lift, runExceptT, throwError)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.WorkspaceByPos
-- >
-- > myManageHook = workspaceByPos <+> manageHook def
-- >
-- > main = xmonad def { manageHook = myManageHook }

workspaceByPos :: ManageHook
workspaceByPos = (maybe idHook doShift <=< liftX . needsMoving) =<< ask

needsMoving :: Window -> X (Maybe WorkspaceId)
needsMoving w = safeGetWindowAttributes w >>= \case
    Nothing -> pure Nothing
    Just wa -> fmap (either (const Nothing) Just) . runExceptT $ do
        -- only relocate windows with non-zero position
        guard $ wa_x wa /= 0 || wa_y wa /= 0
        ws <- gets windowset
        sc <- lift $ fromMaybe (W.current ws)
                <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
        Just wkspc <- lift $ screenWorkspace (W.screen sc)
        guard $ W.currentTag ws /= wkspc
        return wkspc `asTypeOf` throwError ""
