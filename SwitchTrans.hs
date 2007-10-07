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
-- receive any messages; any message not handled by @SwitchTrans@ itself will
-- undo the current layout transformer, pass the message on to the base layout,
-- then reapply the transformer.
--
-- Another potential problem is that functions can't be (de-)serialized so this
-- layout will not preserve state across xmonad restarts.
--
-- Here's how you might use this in Config.hs:
--
-- > defaultLayouts =
-- >     map (
-- >         mkSwitch (M.fromList [
-- >             ("full", const $ Layout $ noBorders Full)
-- >         ]) .
-- >         mkSwitch (M.fromList [
-- >             ("mirror", Layout . Mirror)
-- >         ])
-- >     ) [ Layout tiled ]
--
-- (The @noBorders@ transformer is from "XMonadContrib.NoBorders".)
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
-- The reason I use two stacked @SwitchTrans@ transformers instead of @mkSwitch
-- (M.fromList [(\"full\", const $ Layout $ noBorders Full), (\"mirror\",
-- Layout . Mirror)])@ is that I use @mod-f@ to \"zoom in\" on interesting
-- windows, no matter what other layout transformers may be active. Having an
-- extra fullscreen mode on top of everything else means I can zoom in and out
-- without implicitly undoing \"normal\" layout transformers, like @Mirror@.
-- Remember, inside a @SwitchTrans@ there can be at most one active layout
-- transformer.
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

-- import System.IO


-- | Toggle the specified layout transformer.
data Toggle = Toggle String deriving (Eq, Typeable)
instance Message Toggle
-- | Enable the specified transformer.
data Enable = Enable String deriving (Eq, Typeable)
instance Message Enable
-- | Disable the specified transformer.
data Disable = Disable String deriving (Eq, Typeable)
instance Message Disable

data SwitchTrans a = SwitchTrans {
    base :: Layout a,
    currTag :: Maybe String,
    currLayout :: Layout a,
    currFilt :: Layout a -> Layout a,
    filters :: Map String (Layout a -> Layout a)
}

instance Show (SwitchTrans a) where
    show st = "SwitchTrans #<base: " ++ show (base st) ++ ", tag: " ++ show (currTag st) ++ ", layout: " ++ show (currLayout st) ++ ", ...>"

instance Read (SwitchTrans a) where
    readsPrec _ _ = []

unLayout :: Layout a -> (forall l. (LayoutClass l a) => l a -> r) -> r
unLayout (Layout l) k = k l

acceptChange :: (LayoutClass l a) => SwitchTrans a -> ((l a -> SwitchTrans a) -> b -> c) -> X b -> X c
acceptChange st f action =
    -- seriously, Dave, you need to stop this
    fmap (f (\l -> st{ currLayout = Layout l})) action

instance LayoutClass SwitchTrans a where
    description _ = "SwitchTrans"

    doLayout st r s = currLayout st `unLayout` \l ->
        acceptChange st (fmap . fmap) (doLayout l r s)

    pureLayout st r s = currLayout st `unLayout` \l -> pureLayout l r s

    handleMessage st m
        | Just (Disable tag) <- fromMessage m
        , M.member tag (filters st)
            = provided (currTag st == Just tag) $ disable
        | Just (Enable tag) <- fromMessage m
        , Just alt <- M.lookup tag (filters st)
            = provided (currTag st /= Just tag) $ enable tag alt
        | Just (Toggle tag) <- fromMessage m
        , Just alt <- M.lookup tag (filters st)
            =
            if (currTag st == Just tag) then
                disable
            else
                enable tag alt
        | Just ReleaseResources <- fromMessage m
            = currLayout st `unLayout` \cl ->
                acceptChange st fmap (handleMessage cl m)
        | Just Hide <- fromMessage m
            = currLayout st `unLayout` \cl ->
                acceptChange st fmap (handleMessage cl m)
        | otherwise = base st `unLayout` \b -> do
            x <- handleMessage b m
            case x of
                Nothing -> return Nothing
                Just b' -> currLayout st `unLayout` \cl -> do
                    handleMessage cl (SomeMessage ReleaseResources)
                    let b'' = Layout b'
                    return . Just $ st{ base = b'', currLayout = currFilt st b'' }
        where
        enable tag alt = currLayout st `unLayout` \cl -> do
            -- io $ hPutStrLn stderr $ "[ST]+ " ++ show cl ++ " -> " ++ show (alt (base st))
            handleMessage cl (SomeMessage ReleaseResources)
            return . Just $ st{
                currTag = Just tag,
                currFilt = alt,
                currLayout = alt (base st) }
        disable = currLayout st `unLayout` \cl -> do
            -- io $ hPutStrLn stderr $ "[ST]- " ++ show cl ++ " -> " ++ show (base st)
            handleMessage cl (SomeMessage ReleaseResources)
            return . Just $ st{
                currTag = Nothing,
                currFilt = id,
                currLayout = base st }

-- | Take a transformer table and a base layout, and return a
-- SwitchTrans layout.
mkSwitch :: Map String (Layout a -> Layout a) -> Layout a -> Layout a
mkSwitch fs b = Layout st
    where
    st = SwitchTrans{
        base = b,
        currTag = Nothing,
        currLayout = b,
        currFilt = id,
        filters = fs }
        
provided :: Bool -> X (Maybe a) -> X (Maybe a)
provided c x
    | c = x
    | otherwise = return Nothing

