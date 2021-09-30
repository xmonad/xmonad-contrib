{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Square
-- Description :  A layout that splits the screen into a square area and the rest of the screen.
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that splits the screen into a square area and the rest of the
-- screen.
-- This is probably only ever useful in combination with
-- "XMonad.Layout.Combo".
-- It sticks one window in a square region, and makes the rest
-- of the windows live with what's left (in a full-screen sense).
--
-----------------------------------------------------------------------------

module XMonad.Layout.Square (
                             -- * Usage
                             -- $usage
                             Square(..) ) where

import XMonad
import XMonad.StackSet ( integrate )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- >   import XMonad.Layout.Square
--
-- An example layout using square together with "XMonad.Layout.Combo"
-- to make the very last area square:
--
-- > , combo (combo (mirror $ twoPane 0.03 0.85),1)] (twoPane 0.03 0.5) )
-- >                [(twoPane 0.03 0.2,1),(combo [(twoPane 0.03 0.8,1),(square,1)]
-- >         [(tabbed,3),(tabbed,30),(tabbed,1),(tabbed,1)]

-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Square a = Square deriving ( Read, Show )

instance LayoutClass Square a where
    pureLayout Square r s = arrange (integrate s)
        where arrange ws@(_:_) = map (, rest) (init ws) ++ [(last ws,sq)]
              arrange [] = [] -- actually, this is an impossible case
              (rest, sq) = splitSquare r

splitSquare :: Rectangle -> (Rectangle, Rectangle)
splitSquare (Rectangle x y w h)
    | w > h = (Rectangle x y (w - h) h, Rectangle (x+fromIntegral (w-h)) y h h)
    | otherwise = (Rectangle x y w (h-w), Rectangle x (y+fromIntegral (h-w)) w w)
