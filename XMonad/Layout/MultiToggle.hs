{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiToggle
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dynamically apply and unapply transformers to your window layout. This can
-- be used to rotate your window layout by 90 degrees, or to make the
-- currently focused window occupy the whole screen (\"zoom in\") then undo
-- the transformation (\"zoom out\").

module XMonad.Layout.MultiToggle (
    -- * Usage
    -- $usage
    Transformer(..),
    Toggle(..),
    (??),
    EOT(..),
    single,
    mkToggle,
    mkToggle1
) where

import XMonad

import XMonad.StackSet (Workspace(..))

import Control.Arrow
import Data.Typeable
import Data.Maybe

-- $usage
-- The basic idea is to have a base layout and a set of layout transformers,
-- of which at most one is active at any time. Enabling another transformer
-- first disables any currently active transformer; i.e. it works like a
-- group of radio buttons.
--
-- A side effect of this meta-layout is that layout transformers no longer
-- receive any messages; any message not handled by MultiToggle itself will
-- undo the current layout transformer, pass the message on to the base
-- layout, then reapply the transformer.
--
-- To use this module, you need some data types which represent
-- transformers; for some commonly used transformers (including
-- MIRROR, NOBORDERS, and FULL used in the examples below) you can
-- simply import "XMonad.Layout.MultiToggle.Instances".
--
-- Somewhere else in your file you probably have a definition of @layout@;
-- the default looks like this:
--
-- > layout = tiled ||| Mirror tiled ||| Full
--
-- After changing this to
--
-- > layout = mkToggle (single MIRROR) (tiled ||| Full)
--
-- you can now dynamically apply the 'XMonad.Layout.Mirror' transformation:
--
-- > ...
-- >   , ((modMask,               xK_x     ), sendMessage $ Toggle MIRROR)
-- > ...
--
-- (That should be part of your key bindings.) When you press @mod-x@, the
-- active layout is mirrored. Another @mod-x@ and it's back to normal.
--
-- It's also possible to stack @MultiToggle@s.  For example:
--
-- @
-- layout = id
--     . 'XMonad.Layout.NoBorders.smartBorders'
--     . mkToggle (NOBORDERS ?? FULL ?? EOT)
--     . mkToggle (single MIRROR)
--     $ tiled ||| 'XMonad.Layout.Grid.Grid' ||| 'XMonad.Layout.Circle.Circle'
-- @
--
-- By binding a key to @(sendMessage $ Toggle FULL)@ you can temporarily
-- maximize windows, in addition to being able to rotate layouts and remove
-- window borders.
--
-- You can also define your own transformers by creating a data type
-- which is an instance of the 'Transformer' class.  For example, here
-- is the definition of @MIRROR@:
--
-- > data MIRROR = MIRROR deriving (Read, Show, Eq, Typeable)
-- > instance Transformer MIRROR Window where
-- >     transform _ x k = k (Mirror x)
--
-- Note, you need to put @{-\# LANGUAGE DeriveDataTypeable \#-}@ at the
-- beginning of your file.

-- | A class to identify custom transformers (and look up transforming
-- functions by type).
class (Eq t, Typeable t) => Transformer t a | t -> a where
    transform :: (LayoutClass l a) => t -> l a -> (forall l'. (LayoutClass l' a) => l' a -> b) -> b

data EL a = forall l. (LayoutClass l a) => EL (l a)

unEL :: EL a -> (forall l. (LayoutClass l a) => l a -> b) -> b
unEL (EL x) k = k x

transform' :: (Transformer t a) => t -> EL a -> EL a
transform' t el = el `unEL` \l -> transform t l EL

-- | Toggle the specified layout transformer.
data Toggle a = forall t. (Transformer t a) => Toggle t
    deriving (Typeable)

instance (Typeable a) => Message (Toggle a)

data MultiToggleS ts l a = MultiToggleS (l a) (Maybe Int) ts
    deriving (Read, Show)

data MultiToggle ts l a = MultiToggle{
    baseLayout :: l a,
    currLayout :: EL a,
    currIndex :: Maybe Int,
    currTrans :: EL a -> EL a,
    transformers :: ts
}

expand :: (LayoutClass l a, HList ts a) => MultiToggleS ts l a -> MultiToggle ts l a
expand (MultiToggleS b i ts) =
    resolve ts (fromMaybe (-1) i) id
        (\x mt ->
            let g = transform' x in
            mt{
                currLayout = g . EL $ baseLayout mt,
                currTrans = g
            }
        )
        (MultiToggle b (EL b) i id ts)

collapse :: MultiToggle ts l a -> MultiToggleS ts l a
collapse mt = MultiToggleS (baseLayout mt) (currIndex mt) (transformers mt)

instance (LayoutClass l a, Read (l a), HList ts a, Read ts) => Read (MultiToggle ts l a) where
    readsPrec p s = map (first expand) $ readsPrec p s

instance (Show ts, Show (l a)) => Show (MultiToggle ts l a) where
    showsPrec p = showsPrec p . collapse

-- | Construct a @MultiToggle@ layout from a transformer table and a base
-- layout.
mkToggle :: (LayoutClass l a) => ts -> l a -> MultiToggle ts l a
mkToggle ts l = MultiToggle l (EL l) Nothing id ts

-- | Construct a @MultiToggle@ layout from a single transformer and a base
-- layout.
mkToggle1 :: (LayoutClass l a) => t -> l a -> MultiToggle (HCons t EOT) l a
mkToggle1 t = mkToggle (single t)

-- | Marks the end of a transformer list.
data EOT = EOT deriving (Read, Show)
data HCons a b = HCons a b deriving (Read, Show)

infixr 0 ??
-- | Prepend an element to a heterogeneous list. Used to build transformer
-- tables for 'mkToggle'.
(??) :: (HList b w) => a -> b -> HCons a b
(??) = HCons

-- | Construct a singleton transformer table.
single :: a -> HCons a EOT
single = (?? EOT)

class HList c a where
    find :: (Transformer t a) => c -> t -> Maybe Int
    resolve :: c -> Int -> b -> (forall t. (Transformer t a) => t -> b) -> b

instance HList EOT w where
    find EOT _ = Nothing
    resolve EOT _ d _ = d

instance (Transformer a w, HList b w) => HList (HCons a b) w where
    find (HCons x xs) t
        | t `geq` x = Just 0
        | otherwise = fmap succ (find xs t)
    resolve (HCons x xs) n d k =
        case n `compare` 0 of
            LT -> d
            EQ -> k x
            GT -> resolve xs (pred n) d k

geq :: (Typeable a, Eq a, Typeable b) => a -> b -> Bool
geq a b = Just a == cast b

acceptChange :: (LayoutClass l' a) => MultiToggle ts l a -> ((l' a -> MultiToggle ts l a) -> b -> c) -> X b -> X c
acceptChange mt f = fmap (f (\x -> mt{ currLayout = EL x }))

instance (Typeable a, Show ts, HList ts a, LayoutClass l a) => LayoutClass (MultiToggle ts l) a where
    description mt = currLayout mt `unEL` \l -> description l

    runLayout (Workspace i mt s) r
        | isNothing (currIndex mt) =
            acceptChange mt (fmap . fmap . \f x -> (f x){ baseLayout = x }) $ runLayout (Workspace i (baseLayout mt) s) r
        | otherwise = currLayout mt `unEL` \l ->
            acceptChange mt (fmap . fmap) $ runLayout (Workspace i l s) r

    handleMessage mt m
        | Just (Toggle t) <- fromMessage m
        , i@(Just _) <- find (transformers mt) t
            = currLayout mt `unEL` \l ->
            if i == currIndex mt
                then do
                    handleMessage l (SomeMessage ReleaseResources)
                    return . Just $
                        mt{
                            currLayout = EL $ baseLayout mt,
                            currIndex = Nothing,
                            currTrans = id
                        }
                else do
                    handleMessage l (SomeMessage ReleaseResources)
                    let f = transform' t
                    return . Just $
                        mt{
                            currLayout = f . EL $ baseLayout mt,
                            currIndex = i,
                            currTrans = f
                        }
        | fromMessage m == Just ReleaseResources ||
          fromMessage m == Just Hide
            = currLayout mt `unEL` \l -> acceptChange mt fmap (handleMessage l m)
        | otherwise = do
            ml <- handleMessage (baseLayout mt) m
            case ml of
                Nothing -> return Nothing
                Just b' -> currLayout mt `unEL` \l -> do
                    handleMessage l (SomeMessage ReleaseResources)
                    return . Just $
                        mt{ baseLayout = b', currLayout = currTrans mt . EL $ b' }
