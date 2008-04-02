{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.HintedGrid
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
    Grid(..), arrange
) where

import Prelude hiding ((.))

import XMonad hiding (windows)
import XMonad.StackSet

import Control.Monad.State
import Data.List
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
-- > myLayouts = Grid False ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see
-- "XMonad.Doc.Extending#Editing_the_layout_hook".

-- | Automatic mirroring of hinted layouts doesn't work very well, so this
-- 'Grid' comes with built-in mirroring. @Grid False@ is the normal layout,
-- @Grid True@ is the mirrored variant (rotated by 90 degrees).
data Grid a = Grid Bool deriving (Read, Show)

instance LayoutClass Grid Window where
    doLayout (Grid m) r w = flip (,) Nothing . arrange m r (integrate w)

adjBorders :: Dimension -> D -> D
adjBorders b (w, h) = (w + 2 * b, h + 2 * b)

replicateS :: Int -> (a -> (b, a)) -> a -> ([b], a)
replicateS n = runState . replicateM n . State

doColumn :: Dimension -> Dimension -> Dimension -> [(D -> D)] -> [D]
doColumn width height k adjs =
    let
        (ind, fs) = unzip . sortBy (comparing $ snd . ($ (width, height)) . snd) . zip [0 :: Int ..] $ adjs
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
            xs = map ((width' +) . (`div` 2) . (maxw -)) $ ws
        in
        zipWith3 (\x y (w, h) -> Rectangle (fromIntegral x) (fromIntegral y) w h) xs ys c' ++ doR width' (n - 1) cs

-- | The internal function for computing the grid layout.
arrange :: Bool -> Rectangle -> [Window] -> X [(Window, Rectangle)]
arrange mirror (Rectangle rx ry rw rh) windows = do
    dpy <- asks display
    hints <- mapM (io . getWMNormalHints dpy) windows
    borders <- mapM (io . fmap (fromIntegral . wa_border_width) . getWindowAttributes dpy) windows
    let
        adjs = zipWith (\h b -> twist . adjBorders b . applySizeHints h . adjBorders (negate b) . twist) hints borders
        rs = arrange' (twist (rw, rh)) adjs
        rs' = map (\(Rectangle x y w h) -> uncurry (uncurry Rectangle (twist (x, y))) (twist (w, h))) rs
    return . zip windows . map (\r -> r{ rect_x = rect_x r + rx, rect_y = rect_y r + ry }) $ rs'
    where
    twist
        | mirror = \(a, b) -> (b, a)
        | otherwise = id

arrange' :: D -> [D -> D] -> [Rectangle]
arrange' (rw, rh) adjs = reverse $ doRect rh rw (fromIntegral ncolumns) (ecols ++ cols)
    where
    nwindows = length adjs
    ncolumns = max 1 . round . sqrt $ fromIntegral nwindows * fromIntegral rw / (fromIntegral rh :: Double)
    nrows = nwindows `div` ncolumns
    nextras = nwindows - ncolumns * nrows
    (ecols, adjs') = replicateS nextras (splitAt (nrows + 1)) $ reverse adjs
    (cols, _) = replicateS (ncolumns - nextras) (splitAt nrows) adjs'
