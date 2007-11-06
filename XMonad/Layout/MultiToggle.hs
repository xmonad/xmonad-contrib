{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiToggle
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable


module XMonad.Layout.MultiToggle (
    EL(..),
    unEL,
    LayoutTransformer(..),
    Toggle(..),
    (.*.),
    HNil(..),
    mkToggle
) where


import XMonad

import Control.Arrow
import Data.Typeable
import Data.Maybe

data EL a = forall l. (LayoutClass l a) => EL (l a)

unEL :: EL a -> (forall l. (LayoutClass l a) => l a -> b) -> b
unEL (EL x) k = k x

class (Eq t, Typeable t) => LayoutTransformer t a | t -> a where
    transform :: t -> EL a -> EL a

data Toggle a = forall t. (LayoutTransformer t a) => Toggle t
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
            let g = transform x in
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

mkToggle :: (LayoutClass l a) => ts -> l a -> MultiToggle ts l a
mkToggle ts l = MultiToggle l (EL l) Nothing id ts

data HNil = HNil deriving (Read, Show)
data HCons a b = HCons a b deriving (Read, Show)

infixr 0 .*.
(.*.) :: (HList b w) => a -> b -> HCons a b
(.*.) = HCons

class HList c a where
    find :: (LayoutTransformer t a) => c -> t -> Maybe Int
    resolve :: c -> Int -> b -> (forall t. (LayoutTransformer t a) => t -> b) -> b

instance HList HNil w where
    find HNil _ = Nothing
    resolve HNil _ d _ = d

instance (LayoutTransformer a w, HList b w) => HList (HCons a b) w where
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
    description _ = "MultiToggle"

    pureLayout mt r s = currLayout mt `unEL` \l -> pureLayout l r s

    doLayout mt r s = currLayout mt `unEL` \l -> acceptChange mt (fmap . fmap) (doLayout l r s)

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
                    let f = transform t
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
