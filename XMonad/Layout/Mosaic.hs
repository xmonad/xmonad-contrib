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
-- Based on MosaicAlt, but aspect ratio messages always change the aspect
-- ratios, and rearranging the window stack changes the window sizes.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Mosaic (
    -- * Usage
    -- $usage
    Mosaic(Mosaic)
    ,Aspect(..)
    ,shallower
    ,steeper
    )
    where

import Prelude hiding (sum)

import XMonad(Typeable,
              LayoutClass(doLayout , pureMessage, description), Message,
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
-- > myLayouts = Mosaic [4..12] ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- Unfortunately, infinite lists break serialization, so don't use them.
--
-- To change the choice in aspect ratio and the relative sizes of windows, add
-- to your keybindings:
--
--  > , ((modMask, xK_a), sendMessage Taller)
--  > , ((modMask, xK_z), sendMessage Wider)
--  > , ((modMask, xK_h), sendMessage Shrink >> sendMessage (SlopeMod shallower))
--  > , ((modMask, xK_l), sendMessage Expand >> sendMessage (SlopeMod steeper))
--
--  > , ((modMask, xK_r), sendMessage Reset)
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
    {- | The relative magnitudes of the positive rational numbers provided
         determine the relative sizes of the windows. If the numbers are all
         the same, then the layout looks like Grid. An increasing list results
         in the master window being the largest. Only as many windows are
         displayed as there are elements in that list
    -}
    = Mosaic [Rational]
    -- override the aspect? current index, maximum index
    | MosaicSt Bool Rational Int [Rational]
    deriving (Read, Show)

instance LayoutClass Mosaic a where
    description = const "Mosaic"

    pureMessage (Mosaic _ss) _ms = Nothing
    pureMessage (MosaicSt _ ix mix ss) ms = fromMessage ms >>= ixMod
        where ixMod Taller | rix >= mix = Nothing
                           | otherwise = Just $ MosaicSt False (succ ix) mix ss
              ixMod Wider  | rix <= 0   = Nothing
                           | otherwise = Just $ MosaicSt False (pred ix) mix ss
              ixMod Reset              = Just $ Mosaic ss
              ixMod (SlopeMod f)       = Just $ MosaicSt False ix mix (f ss)
              rix = round ix

    doLayout (Mosaic ss) r st = return (zip (integrate st) rect, newLayout)
        where rects = splits (length $ integrate st) r ss
              lrects = length rects
              rect = rects !! (lrects `div` 2)
              newLayout = Just $ MosaicSt True (fromIntegral lrects / 2) (pred lrects) ss

    doLayout (MosaicSt override ix mix ss) r st
                                = return (zip (integrate st) rect, newLayout)
        where rects = splits (length $ integrate st) r ss
              lrects = length rects
              nix = if mix == 0 || override then fromIntegral $ lrects `div` 2
                        else max 0 $ min (fromIntegral $ pred lrects)
                            $ fromIntegral (pred lrects) * ix / fromIntegral mix
              rect = rects !! round nix
              newLayout = Just $ MosaicSt override nix (pred lrects) ss

-- | These sample functions scale the ratios of successive windows, other
-- variations could also be useful.
--
-- The windows in each position of the stack should correspond to a specific
-- element of the list, so it should be possible to resize individual windows,
-- though it is not yet provided.
steeper :: [Rational] -> [Rational]
steeper [] = []
steeper (x:xs) = map (subtract (x*0.8)) (x:xs)

shallower :: [Rational] -> [Rational]
shallower [] = []
shallower (x:xs) = map (+(x/0.8)) (x:xs)

splits :: Int -> Rectangle -> [Rational] -> [[Rectangle]]
splits num rect sz = splitsL rect $ makeTree $ normalize $ take num sz

normalize :: Fractional a => [a] -> [a]
normalize x = let s = sum x
    in map (/s) x

-- recursively enumerate splits
splitsL :: Rectangle -> Tree Rational -> [[Rectangle]]
splitsL _rect Empty = []
splitsL rect (Leaf _) = [[rect]]
splitsL rect (Branch l r) = do
    let mkSplit f = f (sum l / (sum l + sum r)) rect
    (rl,rr) <- map mkSplit [splitVerticallyBy,splitHorizontallyBy]
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

