{-# LANGUAGE MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
{- |

Module      :  XMonad.Layout.TrackFloating
Copyright   :  (c) 2010 Adam Vogt
License     :  BSD-style (see xmonad/LICENSE)

Maintainer  :  vogt.adam@gmail.com
Stability   :  unstable
Portability :  unportable

Layout modifier that tracks focus in the tiled layer while the floating layer
is in use. This is particularly helpful for tiled layouts where the focus
determines what is visible.

The relevant bug is Issue 4
<http://code.google.com/p/xmonad/issues/detail?id=4>. Explanation:

Focus in the tiled layer goes to the first window in the stack (so-called
master window) when you focus the tiled layer.

See 'trackFloating' for usage.

-}
module XMonad.Layout.TrackFloating
    (trackFloating,
     TrackFloating,
    ) where

import Control.Monad
import Data.List
import qualified Data.Map as M

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W


data TrackFloating a = TrackFloating (Maybe Window)
    deriving (Read,Show)


instance LayoutModifier TrackFloating Window where
    modifyLayoutWithUpdate (TrackFloating mw) ws@(W.Workspace{ W.stack = ms }) r
      = do
        winset <- gets windowset
        let sTotal = W.stack $ W.workspace $ W.current winset
            newStack
              -- focus is floating, so use the remembered focus point
              | Just sTotal' <- sTotal,
                W.focus sTotal' `M.member` W.floating winset,
                Just w <- mw,
                Just s <- ms,
                Just ns <- find ((==) w . W.focus)
                    $ zipWith const (iterate W.focusDown' s) (W.integrate s)
                = Just ns
              | otherwise
                = ms
            newState
              | Just sTotal' <- sTotal
                = if W.focus sTotal' `M.member` W.floating winset
                    then mw
                    else Just (W.focus sTotal')
              | otherwise
                = Nothing
        ran <- runLayout ws{ W.stack = newStack } r
        return (ran, guard (newState /= mw) >> Just (TrackFloating newState))


{- | Apply to your layout in a config like:

> main = xmonad (defaultConfig{
>                   layoutHook = trackFloating
>                       (noBorders Full ||| Tall 1 0.3 0.5),
>                   ...
>               })

Interactions with some layout modifiers (ex. decorations, minimizing) are
unknown but likely unpleasant.
-}
trackFloating ::  l a -> ModifiedLayout TrackFloating l a
trackFloating layout = ModifiedLayout (TrackFloating Nothing) layout

