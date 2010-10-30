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
import Data.Maybe
import qualified Data.Map as M

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W


data TrackFloating a = TrackFloating
    { _wasFloating :: Bool,
      _tiledFocus :: Maybe Window }
    deriving (Read,Show,Eq)


instance LayoutModifier TrackFloating Window where
    modifyLayoutWithUpdate os@(TrackFloating wasF mw) ws@(W.Workspace{ W.stack = ms }) r
      = do
        winset <- gets windowset
        let sCur = fmap W.focus $ W.stack $ W.workspace $ W.current winset
            isF = fmap (`M.member` W.floating winset) sCur
            newStack
              -- focus is floating, so use the remembered focus point
              | Just isF' <- isF,
                isF' || wasF,
                Just w <- mw,
                Just s <- ms,
                Just ns <- find ((==) w . W.focus)
                    $ zipWith const (iterate W.focusDown' s) (W.integrate s)
                = Just ns
              | otherwise
                = ms
            newState = case isF of
              Just True -> mw
              Just False | Just f <- sCur -> Just f
              _ -> Nothing
        ran <- runLayout ws{ W.stack = newStack } r
        return (ran,
                let n = TrackFloating (fromMaybe False isF) newState
                in guard (n /= os) >> Just n)


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
trackFloating layout = ModifiedLayout (TrackFloating False Nothing) layout

