{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{- |

Module      :  XMonad.Layout.TrackFloating
Description :  Let focused tiles track focused floats
Copyright   :  (c) 2010 & 2013 Adam Vogt
               2011 Willem Vanlint
License     :  BSD-style (see xmonad/LICENSE)

Maintainer  :  vogt.adam@gmail.com
Stability   :  unstable
Portability :  unportable

Provides layout modifier 'UseTransientFor': when a float has focus and is
@WM_TRANSIENT_FOR@ a tile, run the underlying layout as if that tile had focus.

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

import XMonad.Prelude
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.FocusTracking
import XMonad.Util.Stack (findZ)
import qualified XMonad.StackSet as W

import qualified Data.Traversable as T


{-# DEPRECATED TrackFloating "Use X.L.FocusTracking.FocusTracking." #-}
type TrackFloating = FocusTracking


{- | When focus is on the tiled layer, the underlying layout is run with focus
on the window named by the WM_TRANSIENT_FOR property on the floating window.
-}
useTransientFor :: l a -> ModifiedLayout UseTransientFor l a
useTransientFor = ModifiedLayout UseTransientFor

data UseTransientFor a = UseTransientFor deriving (Read,Show,Eq)

instance LayoutModifier UseTransientFor Window where
    modifyLayout _ ws@W.Workspace{ W.stack = ms } r = do
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
{-# DEPRECATED trackFloating "Use X.L.FocusTracking.focusTracking." #-}
trackFloating ::  l a -> ModifiedLayout TrackFloating l a
trackFloating = focusTracking

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
