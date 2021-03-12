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
or another sublayout is in use. This is particularly helpful for tiled layouts
where the focus determines what is visible. It can also be used to improve the
behaviour of a child layout that has not been given the focused window.

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

import Control.Applicative ((<|>))
import Control.Monad

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.Stack (findZ)
import qualified XMonad.StackSet as W

import qualified Data.Traversable as T


data TrackFloating a = TrackFloating (Maybe Window)
    deriving (Read,Show)


instance LayoutModifier TrackFloating Window where
    modifyLayoutWithUpdate (TrackFloating mw) ws@(W.Workspace{ W.stack = ms }) r
      = do
        xCur <- gets (W.peek . W.view (W.tag ws) . windowset)
        let isF = xCur /= (W.focus <$> ms)
            -- use the remembered focus point when true focus differs from
            -- what this (sub)layout is given, which happens e.g. when true
            -- focus is in floating layer or when another sublayout has focus
            newStack | isF = (mw >>= \w -> findZ (w==) ms) <|> ms
                     | otherwise = ms
            newState | isF = mw
                     | otherwise = xCur
        ran <- runLayout ws{ W.stack = newStack } r
        return (ran, guard (newState /= mw) >> Just (TrackFloating newState))



{- | When focus is on the tiled layer, the underlying layout is run with focus
on the window named by the WM_TRANSIENT_FOR property on the floating window.
-}
useTransientFor :: l a -> ModifiedLayout UseTransientFor l a
useTransientFor x = ModifiedLayout UseTransientFor x

data UseTransientFor a = UseTransientFor deriving (Read,Show,Eq)

instance LayoutModifier UseTransientFor Window where
    modifyLayout _ ws@(W.Workspace{ W.stack = ms }) r = do
        m <- gets (W.peek . W.view (W.tag ws) . windowset)
        d <- asks display
        parent <- join <$> T.traverse (io . getTransientForHint d) m

        s0 <- get
        whenJust parent $ \p -> put s0{ windowset = W.focusWindow p (windowset s0) }
        result <- runLayout ws{ W.stack = (parent >>= \p -> findZ (p==) ms) <|> ms } r

        m' <- gets (W.peek . windowset)

        when (m' == parent) $
            -- layout changed the windowset, so don't clobber it
            whenJust m $ \p -> put s0{ windowset = W.focusWindow p (windowset s0) }

        return result



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
trackFloating layout = ModifiedLayout (TrackFloating Nothing) layout

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
