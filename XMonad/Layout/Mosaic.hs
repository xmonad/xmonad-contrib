{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
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
-- Based on MosaicAlt, but aspect ratio messages allways change the aspect
-- ratios, and rearranging the window stack changes the window sizes.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Mosaic (
    Mosaic(..)
    ,Aspect(..)
    )
    where

import Prelude hiding (sum)

import XMonad(Typeable,
              LayoutClass(pureLayout, pureMessage, description), Message,
              fromMessage, splitHorizontallyBy, splitVerticallyBy, Rectangle)
import XMonad.StackSet(integrate)
import Data.Foldable(Foldable(foldMap), sum)
import Data.Monoid(Monoid(mappend, mempty))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Mosaic
--
-- Then edit your @layoutHook@ by adding the Mosaic layout:
--
-- > myLayouts = Mosaic 0 [1..10] ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- The numbers are directly proportional to the area given, with the
-- master window getting the most if you have an ascending list.
--
-- Unfortunately, infinite lists break serialization, so
-- don't use them
--
-- The position of a window in the stack determines its size.
--
-- To change the choice in aspect ratio, add to your keybindings:
--
--  , ((modMask, xK_a), sendMessage Taller)
--  , ((modMask, xK_z), sendMessage Wider)
--  , ((modMask, xK_s), sendMessage (SlopeMod (zipWith (*) [1..])))
--  , ((modMask, xK_d), sendMessage (SlopeMod (zipWith (flip (/)) [1..])))
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

data Mosaic a
    = Mosaic Int [Rational]
    deriving (Read, Show)

instance LayoutClass Mosaic a where
    description = const "Mosaic"

    pureMessage (Mosaic i ss) msg = ixMod $ fromMessage msg
        where ixMod (Just Wider) = Just $ Mosaic (succ i) ss
              ixMod (Just Taller)  = if i <= 1 then Nothing else Just $ Mosaic (pred i) ss
              ixMod (Just Reset) = Just $ Mosaic 0 ss
              ixMod (Just (SlopeMod f)) = Just $ Mosaic i (f ss)
              ixMod _ = Nothing

    pureLayout (Mosaic i ss) r st = zip (integrate st) (rect i)
        where rects = splits (length $ integrate st) r ss
              rect 0 = rects !! (length rects `div` 2)
              rect n = if length rects < n then last rects else rects !! pred n

splits :: Int -> Rectangle -> [Rational] -> [[Rectangle]]
splits num rect sz = splitsL rect $ makeTree $ normalize $ take num sz
--    where --fas = normalize $ map (fromIntegral (sum fas')/) $ map fromIntegral fas'

normalize :: Fractional a => [a] -> [a]
normalize x = let s = sum x
    in map (/s) x

-- recursively enumerate splits
splitsL :: Rectangle -> Tree Rational -> [[Rectangle]]
splitsL _rect Empty = []
splitsL rect (Leaf _) = [[rect]]
splitsL rect (Branch l r) = do
    let mkSplit f = f (sum l / (sum l + sum r)) rect
    (rl,rr) <- map mkSplit [splitHorizontallyBy,splitVerticallyBy]
    splitsL rl l `interleave` splitsL rr r

interleave :: [[a]] -> [[a]] -> [[a]]
interleave xs ys | lx > ly = zc xs (extend lx ys)
                 | otherwise = zc (extend ly xs) ys
    where lx = length xs
          ly = length ys
          zc = zipWith (++)

extend :: Int -> [a] -> [a]
extend n pat = do
    (p,e') <- zip pat $ take m (repeat True) ++ repeat False
    let e = if e' then [p] else []
    (e++) $ take d $ repeat p
    where (d,m) = n `divMod` length pat

data Tree a = Branch (Tree a) (Tree a) | Leaf a | Empty
    deriving (Show)

instance Foldable Tree where
   foldMap _f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

instance Monoid (Tree a) where
    mempty = Empty
    mappend Empty x = x
    mappend x Empty = x
    mappend x y = Branch x y

makeTree :: [Rational] -> Tree Rational
makeTree [] = Empty
makeTree [x] = Leaf x
makeTree xs = Branch (makeTree a) (makeTree b)
    where ((a,b),_) = foldr w (([],[]),(0,0)) xs
          w n ((ls,rs),(l,r)) = if l > r then ((ls,n:rs),(l,n+r))
                                        else ((n:ls,rs),(n+l,r))

