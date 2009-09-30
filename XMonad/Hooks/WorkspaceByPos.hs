----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WorkspaceByPos
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
import qualified XMonad.StackSet as W

import Data.Maybe
import Control.Applicative((<$>))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.WorkspaceByPos
-- >
-- > myManageHook = workspaceByPos <+> manageHook defaultConfig
-- >
-- > main = xmonad defaultConfig { manageHook = myManageHook }

workspaceByPos :: ManageHook
workspaceByPos = ask >>= \w -> do
                    b <- liftX $ needsMoving w
                    case b of
                        Nothing       -> idHook
                        Just wkspc    -> doShift wkspc

needsMoving :: Window -> X (Maybe WorkspaceId)
needsMoving w = withDisplay $ \d -> do
                    -- only relocate windows with non-zero position
                    wa <- io $ getWindowAttributes d w
                    if ((wa_x wa) == 0) && ((wa_y wa) == 0)
                        then return Nothing
                        else do
                            ws <- gets windowset
                            sc <- fromMaybe (W.current ws)
                                    <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)
                            maybeWkspc <- screenWorkspace (W.screen sc)
                            case maybeWkspc of
                                Nothing -> return Nothing
                                Just wkspc -> do
                                    let currentWksp = W.currentTag ws
                                    if currentWksp == wkspc
                                        then return Nothing
                                        else return (Just wkspc)

fi ::  (Integral a, Num b) => a -> b
fi = fromIntegral
