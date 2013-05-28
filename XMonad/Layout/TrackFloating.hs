{-# LANGUAGE MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
{- |

Module      :  XMonad.Layout.TrackFloating
Copyright   :  (c) 2010 & 2013 Adam Vogt
               2011 Willem Vanlint
License     :  BSD-style (see xmonad/LICENSE)

Maintainer  :  vogt.adam@gmail.com
Stability   :  unstable
Portability :  unportable

Layout modifier that tracks focus in the tiled layer while the floating layer
is in use. This is particularly helpful for tiled layouts where the focus
determines what is visible.

The relevant bugs are Issue 4 and 306:
<http://code.google.com/p/xmonad/issues/detail?id=4>,
<http://code.google.com/p/xmonad/issues/detail?id=306>
-}
module XMonad.Layout.TrackFloating
    (-- * Usage
     -- $usage

     -- ** For other layout modifiers
     -- $layoutModifier
     trackFloating,
     useTransientFor,

     -- ** Exported types
     TrackFloating,
     UseTransientFor,
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

import qualified Data.Traversable as T


data TrackFloating a = TrackFloating
    { _wasFloating :: Bool,
      _tiledFocus :: Maybe Window }
    deriving (Read,Show,Eq)


instance LayoutModifier TrackFloating Window where
    modifyLayoutWithUpdate os@(TrackFloating _wasF mw) ws@(W.Workspace{ W.stack = ms }) r
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
                isF',
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



{- | When focus is on the tiled layer, the underlying layout is run with focus
on the window named by the WM_TRANSIENT_FOR property on the floating window.
-}
useTransientFor :: l a -> ModifiedLayout UseTransientFor l a
useTransientFor x = ModifiedLayout UseTransientFor x

data UseTransientFor a = UseTransientFor deriving (Read,Show,Eq)

instance LayoutModifier UseTransientFor Window where
    modifyLayout _ ws@(W.Workspace{ W.stack = ms }) r = do
        m <- gets (W.peek . windowset)
        d <- asks display
        parent <- fmap join $ T.traverse (io . getTransientForHint d) m

        s0 <- get
        whenJust parent $ \p -> put s0{ windowset = W.focusWindow p (windowset s0) }
        result <- runLayout ws{ W.stack = fromMaybe ms (liftM2 focusWin ms parent) } r

        m' <- gets (W.peek . windowset)

        when (m' == parent) $
            -- layout changed the windowset, so don't clobber it
            whenJust m $ \p -> put s0{ windowset = W.focusWindow p (windowset s0) }

        return result



focusWin :: Eq a => W.Stack a -> a -> Maybe (W.Stack a)
focusWin st@(W.Stack f u d) w
        | w `elem` u || w `elem` d = Just . head . filter ((==w) . W.focus)
            $ iterate (if w `elem` u then W.focusUp'
                                        else W.focusDown') st
        | w == f = Just st
        | otherwise = Nothing



{- $usage

Apply to your layout in a config like:

> main = xmonad (def{
>                   layoutHook = trackFloating (useTransientFor
>                       (noBorders Full ||| Tall 1 0.3 0.5)),
>                   ...
>               })


'useTransientFor' and 'trackFloating' can be enabled independently.  For
example when the floating window sets @WM_TRANSIENT_FOR@, such as libreoffice's
file->preferences window, @optionA@ will have the last-focused window magnified
while @optionB@ will result magnify the window that opened the preferences
window regardless of which tiled window was focused before.

> import XMonad.Layout.Magnifier
> import XMonad.Layout.TrackFloating
>
> underlyingLayout = magnifier (Tall 1 0.3 0.5)
>
> optionA = trackFloating underlyingLayout
> optionB = trackFloating (useTransientFor underlyingLayout)

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
