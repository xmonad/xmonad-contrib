{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Mosaic
-- Copyright   :  (c) 2009 Adam Vogt, 2007 James Webb
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam<at>gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Based on MosaicAlt, but aspect ratio messages always change the aspect
-- ratios, and rearranging the window stack changes the window sizes.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Mosaic (
    -- * Usage
    -- $usage
     Aspect(..)
    ,mosaic
    ,changeMaster
    ,changeFocused

    ,Mosaic
    )
    where

import Prelude hiding (sum)

import XMonad(Typeable,
              LayoutClass(doLayout, handleMessage, pureMessage, description),
              Message, X, fromMessage, withWindowSet, Resize(..),
              splitHorizontallyBy, splitVerticallyBy, sendMessage, Rectangle)
import qualified XMonad.StackSet as W
import Control.Arrow(second, first)
import Control.Monad(mplus)
import Data.Foldable(sum)
import Data.Function(on)
import Data.List(sortBy)


-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Mosaic
--
-- Then edit your @layoutHook@ by adding the Mosaic layout:
--
-- > myLayout = mosaic 2 [3,2] ||| Full ||| etc..
-- > main = xmonad $ def { layoutHook = myLayout }
--
-- Unfortunately, infinite lists break serialization, so don't use them. And if
-- the list is too short, it is extended with @++ repeat 1@, which covers the
-- main use case.
--
-- To change the choice in aspect ratio and the relative sizes of windows, add
-- to your keybindings:
--
--  > , ((modm, xK_a), sendMessage Taller)
--  > , ((modm, xK_z), sendMessage Wider)
--
--  > , ((modm, xK_r), sendMessage Reset)
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data Aspect
    = Taller
    | Wider
    | Reset
    | SlopeMod ([Rational] -> [Rational])
    deriving (Typeable)

instance Message Aspect

-- | The relative magnitudes (the sign is ignored) of the rational numbers in
-- the second argument determine the relative areas that the windows receive.
-- The first number represents the size of the master window, the second is for
-- the next window in the stack, and so on.
--
-- The list is extended with @++ repeat 1@, so @mosaic 1.5 []@ is like a
-- resizable grid.
--
-- The first parameter is the multiplicative factor to use when responding to
-- the 'Expand' message.
mosaic :: Rational -> [Rational] -> Mosaic a
mosaic = Mosaic Nothing

data Mosaic a = -- | True to override the aspect, current index, maximum index
                Mosaic (Maybe(Bool,Rational,Int)) Rational [Rational] deriving (Read,Show)

instance LayoutClass Mosaic a where
    description = const "Mosaic"

    pureMessage (Mosaic Nothing _ _) _ = Nothing
    pureMessage (Mosaic (Just(_,ix,mix)) delta ss) ms = fromMessage ms >>= ixMod
        where ixMod Taller | round ix >= mix = Nothing
                           | otherwise = Just $ Mosaic (Just(False,succ ix,mix)) delta ss
              ixMod Wider  | round ix <= (0::Integer) = Nothing
                           | otherwise = Just $ Mosaic (Just(False,pred ix,mix)) delta ss
              ixMod Reset              = Just $ Mosaic Nothing delta ss
              ixMod (SlopeMod f)       = Just $ Mosaic (Just(False,ix,mix)) delta (f ss)

    handleMessage l@(Mosaic _ delta _) ms
        | Just Expand <- fromMessage ms = changeFocused (*delta) >> return Nothing
        | Just Shrink <- fromMessage ms = changeFocused (/delta) >> return Nothing
        | otherwise = return $ pureMessage l ms

    doLayout (Mosaic state delta ss) r st = let
        ssExt = zipWith const (ss ++ repeat 1) $ W.integrate st
        rects = splits r ssExt
        nls = length rects
        fi = fromIntegral
        nextIx (ov,ix,mix)
                | mix <= 0 || ov = fromIntegral $ nls `div` 2
                | otherwise = max 0 $ (*fi (pred nls)) $ min 1 $ ix / fi mix
        rect = rects !! maybe (nls `div` 2) round (nextIx <$> state)
        state' = fmap (\x@(ov,_,_) -> (ov,nextIx x,pred nls)) state
                    `mplus` Just (True,fromIntegral nls / 2,pred nls)
        ss' = maybe ss (const ss `either` const ssExt) $ zipRemain ss ssExt
        in return (zip (W.integrate st) rect, Just $ Mosaic state' delta ss')

zipRemain :: [a] -> [b] -> Maybe (Either [a] [b])
zipRemain (_:xs) (_:ys) = zipRemain xs ys
zipRemain [] [] = Nothing
zipRemain [] y = Just (Right y)
zipRemain x [] = Just (Left x)

-- | These sample functions are meant to be applied to the list of window sizes
-- through the 'SlopeMod' message.
changeMaster :: (Rational -> Rational) -> X ()
changeMaster = sendMessage . SlopeMod . onHead

-- | Apply a function to the Rational that represents the currently focused
-- window.
--
-- 'Expand' and 'Shrink' messages are responded to with @changeFocused
-- (*delta)@ or @changeFocused (delta/)@ where @delta@ is the first argument to
-- 'mosaic'.
--
-- This is exported because other functions (ex. @const 1@, @(+1)@) may be
-- useful to apply to the current area.
changeFocused :: (Rational -> Rational) -> X ()
changeFocused f = withWindowSet $ sendMessage . SlopeMod
                    . maybe id (mulIx . length . W.up)
                    . W.stack . W.workspace . W.current
    where mulIx i = uncurry (++) . second (onHead f) . splitAt i

onHead :: (a -> a) -> [a] -> [a]
onHead f = uncurry (++) . first (fmap f) . splitAt 1

splits :: Rectangle -> [Rational] -> [[Rectangle]]
splits rect = map (reverse . map snd . sortBy (compare `on` fst))
                . splitsL rect . makeTree snd . zip [1..]
                . normalize . reverse . map abs

splitsL :: Rectangle -> Tree (Int,Rational) -> [[(Int,Rectangle)]]
splitsL _rect Empty = []
splitsL rect (Leaf (x,_)) = [[(x,rect)]]
splitsL rect (Branch l r) = do
    let mkSplit f = f ((sumSnd l /) $ sumSnd l + sumSnd r) rect
        sumSnd = sum . fmap snd
    (rl,rr) <- map mkSplit [splitVerticallyBy,splitHorizontallyBy]
    splitsL rl l `interleave` splitsL rr r

-- like zipWith (++), but when one list is shorter, its elements are duplicated
-- so that they match
interleave :: [[a]] -> [[a]] -> [[a]]
interleave xs ys | lx > ly = zc xs (extend lx ys)
                 | otherwise = zc (extend ly xs) ys
  where lx = length xs
        ly = length ys
        zc = zipWith (++)

        extend :: Int -> [a] -> [a]
        extend n pat = do
            (p,e) <- zip pat $ replicate m True ++ repeat False
            [p | e] ++ replicate d p
            where (d,m) = n `divMod` length pat

normalize :: Fractional a => [a] -> [a]
normalize x = let s = sum x in map (/s) x

data Tree a = Branch (Tree a) (Tree a) | Leaf a | Empty

instance Foldable Tree where
   foldMap _f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

instance Functor Tree where
   fmap f (Leaf x) = Leaf $ f x
   fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
   fmap _ Empty = Empty

instance Monoid (Tree a) where
    mempty = Empty
    mappend Empty x = x
    mappend x Empty = x
    mappend x y = Branch x y

instance Semigroup (Tree a) where
    (<>) = mappend

makeTree ::  (Num a1, Ord a1) => (a -> a1) -> [a] -> Tree a
makeTree _ [] = Empty
makeTree _ [x] = Leaf x
makeTree f xs = Branch (makeTree f a) (makeTree f b)
    where ((a,b),_) = foldr go (([],[]),(0,0)) xs
          go n ((ls,rs),(l,r))
            | l > r     = ((ls,n:rs),(l,f n+r))
            | otherwise = ((n:ls,rs),(f n+l,r))
