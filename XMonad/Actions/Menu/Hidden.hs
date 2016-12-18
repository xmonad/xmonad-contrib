{-|

Module      : XMonad.Actions.Menu.Hidden
Description : A module that displays Menus by not displaying them.
Copyright   : (C) David Janssen, 2016
License     : BSD3
Maintainer  : David Janssen <janssen.dhj@gmail.com>
Stability   : unstable
Portability : unportable

I wrote this module in error. It's probably better to add "show-or-noshow"
configuration to the renderers themselves, but for now, this is a Menu that
simply doesn't show anything. It will still run normally, but nothing will be
displayed.

-}

module XMonad.Actions.Menu.Hidden where

import           Data.Maybe           (fromJust)
import           XMonad
import           XMonad.Actions.Menu.KeyParse (readStroke)
import           XMonad.Actions.Menu.Core

type HiddenCfg      = ()
type HiddenItem     = Item HiddenCfg
type HiddenMenu     = Menu HiddenCfg
type HiddenRenderer = Renderer HiddenCfg
type HiddenTuple    = (String, String, X())


-- Make a hidden item
hiddenItem :: HiddenTuple -> HiddenItem
hiddenItem (ks, _, x) = Item (fromJust . readStroke $ ks) "" x ()

-- To use as a drop in replacement for any menu that does define per-item
-- defaults, just ignores the 4th tuple value.
hiddenItem' :: (String, String, X (), a) -> HiddenItem
hiddenItem' (ks, _, x, _) = Item (fromJust . readStroke $ ks) "" x ()

-- Make a hidden menu
hiddenMenu :: [HiddenTuple] -> HiddenMenu
hiddenMenu es = Menu (\_ -> return . return $ ()) $ map hiddenItem es

-- To use as a drop in replacement for any menu that does define per-item
-- defaults, so you can easily make a Menu hidden by using this constructor
-- instead
hiddenMenu' :: a -> [(String, String, X(), a)] -> HiddenMenu
hiddenMenu' _ es = Menu (\_ -> return . return $ ()) $ map hiddenItem' es
