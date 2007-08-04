{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SwitchTrans
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
--
-- Ordinary layout transformers are simple and easy to use but inflexible.
-- This module provides a more structured interface to them.
--
-- The basic idea is to have a base layout and a set of layout transformers,
-- of which at most one is active at any time. Enabling another transformer
-- first disables any currently active transformer; i.e. it works like
-- a group of radio buttons.
--
-- A side effect of this meta-layout is that layout transformers no longer
-- receive any messages; any message not handled by @SwitchTrans@ itself
-- will undo the current layout transformer, pass the message on to the base
-- layout, then reapply the transformer.
--
-- Here's how you might use this in Config.hs:
--
-- > defaultLayouts =
-- >     map (
-- >         mkSwitch (M.singleton "full" (const $ noBorders full)) .
-- >         mkSwitch (M.singleton "mirror" mirror)
-- >     ) [ tiled ]
--
-- (The noBorders transformer is from 'XMonadContrib.NoBorders'.)
--
-- This example is probably overkill but it's very close to what I actually use.
-- Anyway, this layout behaves like the default @tiled@ layout, until you send it
-- @Enable@\/@Disable@\/@Toggle@ messages. From the definition of @keys@:
--
-- > ...
-- >    , ((modMask,               xK_f     ), sendMessage $ Toggle "full")
-- >    , ((modMask,               xK_r     ), sendMessage $ Toggle "mirror")
--
-- (You may want to use other keys. I don't use Xinerama so the default mod-r
-- binding is useless to me.)
--
-- After this, pressing @mod-f@ switches the current window to fullscreen mode.
-- Pressing @mod-f@ again switches it back. Similarly, @mod-r@ rotates the layout
-- by 90 degrees (and back). The nice thing is that your changes are kept:
-- Rotating first then changing the size of the master area then rotating back
-- does not undo the master area changes.
--
-- The reason I use two stacked @SwitchTrans@ transformers instead of
-- @mkSwitch (M.fromList [("full", const $ noBorders full), ("mirror", mirror)])@
-- is that I use @mod-f@ to \"zoom in\" on interesting windows, no matter what other
-- layout transformers may be active. Having an extra fullscreen mode on top of
-- everything else means I can zoom in and out without implicitly undoing \"normal\"
-- layout transformers, like @mirror@. Remember, inside a @SwitchTrans@ there can
-- be at most one active layout transformer.
-----------------------------------------------------------------------------

module XMonadContrib.SwitchTrans (
    Toggle(..),
    Enable(..),
    Disable(..),
    mkSwitch
) where

import XMonad
import Operations

import qualified Data.Map as M
import Data.Map (Map)

-- | Toggle the specified layout transformer.
data Toggle = Toggle String deriving (Eq, Typeable)
instance Message Toggle
-- | Enable the specified transformer.
data Enable = Enable String deriving (Eq, Typeable)
instance Message Enable
-- | Disable the specified transformer.
data Disable = Disable String deriving (Eq, Typeable)
instance Message Disable

data State a = State {
    base :: Layout a,
    currTag :: Maybe String,
    currLayout :: Layout a,
    currFilt :: Layout a -> Layout a,
    filters :: Map String (Layout a -> Layout a)
}

-- | Take a transformer table and a base layout, and return a
-- SwitchTrans layout.
mkSwitch :: Map String (Layout a -> Layout a) -> Layout a -> Layout a
mkSwitch fs b = switched st
    where
    st = State{
        base = b,
        currTag = Nothing,
        currLayout = b,
        currFilt = id,
        filters = fs }
        
provided :: Bool -> X (Maybe a) -> X (Maybe a)
provided c x
    | c = x
    | otherwise = return Nothing

switched :: State a -> Layout a
switched
    state@State{
        base = b,
        currTag = ct,
        currLayout = cl,
        currFilt = cf,
        filters = fs
    } = Layout {doLayout = dl, modifyLayout = ml}
    where
    enable tag alt = do
        modifyLayout cl (SomeMessage UnDoLayout)
        return . Just . switched $ state{
            currTag = Just tag,
            currFilt = alt,
            currLayout = alt b }
    disable = do
        modifyLayout cl (SomeMessage UnDoLayout)
        return . Just . switched $ state{
            currTag = Nothing,
            currFilt = id,
            currLayout = b }
    dl r s = do
        (x, _) <- doLayout cl r s
        return (x, Nothing)  -- sorry Dave, I can't let you do that
    ml m
        | Just (Disable tag) <- fromMessage m
        , M.member tag fs
        = provided (ct == Just tag) $ disable
        | Just (Enable tag) <- fromMessage m
        , Just alt <- M.lookup tag fs
        = provided (ct /= Just tag) $ enable tag alt
        | Just (Toggle tag) <- fromMessage m
        , Just alt <- M.lookup tag fs
        =
            if (ct == Just tag) then
                disable
            else
                enable tag alt
        | Just UnDoLayout <- fromMessage m
        = do
            modifyLayout cl m
            return Nothing
        | otherwise = do
            x <- modifyLayout b m
            case x of
                Nothing -> return Nothing
                Just b' -> do
                    modifyLayout cl (SomeMessage UnDoLayout)
                    return . Just $ switched state{
                        base = b',
                        currLayout = cf b' }
