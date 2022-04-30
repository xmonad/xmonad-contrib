-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.ConditionalKeys -- better as PerLayoutKeys not generalized?
-- Copyright   :  (c)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  brandon s allbery kf8ng <allbery.b@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Define key-bindings on per-workspace or per-layout basis.
--
-----------------------------------------------------------------------------

-- stumbled across this at http://lpaste.net/annotate/53022
-- couldn't identify author

-- cf similar at https://github.com/ervandew/dotfiles/blob/master/.xmonad/lib/XMonad/Actions/PerLayoutKeys.hs
-- and http://lpaste.net/44409/perlayoutkeys?pid=44409&lang_44409=cpp

module XMonad.Actions.ConditionalKeys (
                                 -- * Usage
                                 -- $usage
                                 XCond(..),
                                 chooseAction,
                                 bindOn
                                ) where

import XMonad
import qualified XMonad.StackSet as W
import Data.List (find)

-- $usage
-- Place this file in ~/.xmonad/lib/XMonad/Actions/ConditionalKeys.hs
--
--          TODO choose API and module split and document correctly.
--          next paragraph for separate modules, rather than combined
--          even though should merge this and PerWorkspaceKeys.
--
-- See also 'XMonad.Actions.PerWorkspaceKeys' from which this module is shamelessly
-- derived.  -- Use 'XMonad.Layout.Named' to shorten and distinguish the layout
-- descriptions you want to bind conditionally.
--
-- Add something like the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >  import XMonad.Actions.ConditionalKeys
--
-- >  import XMonad.Layout.Named
-- >  import XMonad.Layout.ResizableTile
--
-- >     layoutHook = avoidStruts (named "MRT" (Mirror rt) ||| rt) ||| Full
-- >       where rt = ResizableTall 2 (1/118) (11/15) []
--
-- >     , ("C-M-h", bindOn LD [("MRT", sendMessage MirrorExpand), ("", sendMessage Shrink)]
-- >     , ("C-M-l", bindOn LD [("MRT", sendMessage MirrorShrink), ("", sendMessage Expand)]
-- >     , ("C-M-k", bindOn LD [("MRT", sendMessage Shrink), ("", sendMessage MirrorExpand)]
-- >     , ("C-M-j", bindOn LD [("MRT", sendMessage Expand), ("", sendMessage MirrorShrink)]


data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)


-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()

