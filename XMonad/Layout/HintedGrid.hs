{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.HintedGrid
-- Description :  A layout that puts all windows in a square grid while obeying their size hints.
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A not so simple layout that attempts to put all windows in a square grid
-- while obeying their size hints.
--
-----------------------------------------------------------------------------

module XMonad.Layout.HintedGrid (
    -- * Usage
    -- $usage
    Grid(..), arrange, defaultRatio
) where

import Prelude hiding ((.))

import XMonad
import XMonad.Prelude (replicateM, sortBy, sortOn)
import XMonad.StackSet

import Control.Monad.State (runState)
import Data.Ord

infixr 9 .
(.) :: (Functor f) => (a -> b) -> f a -> f b
(.) = fmap

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.HintedGrid
--
-- Then edit your @layoutHook@ by adding the 'Grid' layout:
--
-- > myLayout = Grid False ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- You can also specify an aspect ratio for Grid to strive for with the
-- GridRatio constructor:
--
-- > myLayout = GridRatio (4/3) False ||| etc.
--
-- For more detailed instructions on editing the layoutHook see
-- "XMonad.Doc.Extending#Editing_the_layout_hook".

-- | Automatic mirroring of hinted layouts doesn't work very well, so this
-- 'Grid' comes with built-in mirroring. @Grid False@ is the normal layout,
-- @Grid True@ is the mirrored variant (rotated by 90 degrees).
data Grid a = Grid Bool | GridRatio Double Bool deriving (Read, Show)

defaultRatio :: Double
defaultRatio = 16/9

instance LayoutClass Grid Window where
    doLayout (Grid m)        r w = doLayout (GridRatio defaultRatio m) r w
    doLayout (GridRatio d m) r w = (, Nothing) . arrange d m r (integrate w)

replicateS :: Int -> (a -> (b, a)) -> a -> ([b], a)
replicateS n f = runState . replicateM n $ do (a,s) <- gets f; put s; return a

doColumn :: Dimension -> Dimension -> Dimension -> [D -> D] -> [D]
doColumn width height k adjs =
    let
        (ind, fs) = unzip . sortOn (snd . ($ (width, height)) . snd) . zip [0 :: Int ..] $ adjs
        (_, ds) = doC height k fs
    in
    map snd . sortBy (comparing fst) . zip ind $ ds
    where
    doC h _ [] = (h, [])
    doC h n (f : fs) = (adj :) . doC (h - h') (n - 1) fs
        where
        adj@(_, h') = f (width, h `div` n)

doRect :: Dimension -> Dimension -> Dimension -> [[D -> D]] -> [Rectangle]
doRect height = doR
    where
    doR _ _ [] = []
    doR width n (c : cs) =
        let
            v = fromIntegral $ length c
            c' = doColumn (width `div` n) height v c
            (ws, hs) = unzip c'
            maxw = maximum ws
            height' = sum hs
            hbonus = height - height'
            hsingle = hbonus `div` v
            hoffset = hsingle `div` 2
            width' = width - maxw
            ys = map ((height -) . subtract hoffset) . scanl1 (+) . map (hsingle +) $ hs
            xs = map ((width' +) . (`div` 2) . (maxw -)) ws
        in
        zipWith3 (\x y (w, h) -> Rectangle (fromIntegral x) (fromIntegral y) w h) xs ys c' ++ doR width' (n - 1) cs

-- | The internal function for computing the grid layout.
arrange :: Double -> Bool -> Rectangle -> [Window] -> X [(Window, Rectangle)]
arrange aspectRatio mirror (Rectangle rx ry rw rh) wins = do
    proto <- mapM mkAdjust wins
    let
        adjs = map (\f -> twist . f . twist) proto
        rs = arrange' aspectRatio (twist (rw, rh)) adjs
        rs' = map (\(Rectangle x y w h) -> uncurry (uncurry Rectangle (twist (x, y))) (twist (w, h))) rs
    return . zip wins . map (\r -> r{ rect_x = rect_x r + rx, rect_y = rect_y r + ry }) $ rs'
    where
    twist
        | mirror = \(a, b) -> (b, a)
        | otherwise = id

arrange' :: Double -> D -> [D -> D] -> [Rectangle]
arrange' aspectRatio (rw, rh) adjs = reverse $ doRect rh rw (fromIntegral ncolumns) (ecols ++ cols)
    where
    nwindows = length adjs
    ncolumns = max 1 . round . sqrt $ fromIntegral nwindows * fromIntegral rw / (fromIntegral rh * aspectRatio)
    nrows = nwindows `div` ncolumns
    nextras = nwindows - ncolumns * nrows
    (ecols, adjs') = replicateS nextras (splitAt (nrows + 1)) $ reverse adjs
    (cols, _) = replicateS (ncolumns - nextras) (splitAt nrows) adjs'
