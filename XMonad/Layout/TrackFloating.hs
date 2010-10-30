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
<http://code.google.com/p/xmonad/issues/detail?id=4>.
-}
module XMonad.Layout.TrackFloating
    (-- * Usage
     -- $usage

     -- ** For other layout modifiers
     -- $layoutModifier
     trackFloating,
     TrackFloating,
    ) where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

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
        let xCur = fmap W.focus xStack
            xStack = W.stack $ W.workspace $ W.current winset
            isF = fmap (\x -> x `M.member` W.floating winset ||
                            (let (\\\) = (S.\\) `on` (S.fromList . W.integrate')
                             in x `S.member` (xStack \\\ ms)))
                        xCur
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
              Just False | Just f <- xCur -> Just f
              _ -> Nothing
        ran <- runLayout ws{ W.stack = newStack } r
        return (ran,
                let n = TrackFloating (fromMaybe False isF) newState
                in guard (n /= os) >> Just n)


{- $usage

Apply to your layout in a config like:

> main = xmonad (defaultConfig{
>                   layoutHook = trackFloating
>                       (noBorders Full ||| Tall 1 0.3 0.5),
>                   ...
>               })

-}

{- | Runs another layout with a remembered focus, provided:

* the subset of windows doesn't include the focus in XState

* it was previously run with a subset that included the XState focus

* the remembered focus hasn't since been killed

-}
trackFloating ::  l a -> ModifiedLayout TrackFloating l a
trackFloating layout = ModifiedLayout (TrackFloating False Nothing) layout

{- $layoutModifier
It also corrects focus issues for full-like layouts inside other layout
modifiers:

> import XMonad.Layout.IM
> import XMonad.Layout.Tabbed
> import XMonad.Layout.TrackFloating
> import XMonad.Layout.Reflect

> gimpLayout = withIM 0.11 (Role "gimp-toolbox") $ reflectHoriz
>       $ withIM 0.15 (Role "gimp-dock") (trackFloating simpleTabbed)

Interactions with some layout modifiers (ex. decorations, minimizing) are
unknown but likely unpleasant.
-}
