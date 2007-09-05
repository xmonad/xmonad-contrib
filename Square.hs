-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Square
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that splits the screen into a square area and the rest of the
-- screen.
-- This is probably only ever useful in combination with 
-- "XMonadContrib.Combo".
-- It sticks one window in a square region, and makes the rest
-- of the windows live with what's left (in a full-screen sense).
--
-----------------------------------------------------------------------------

module XMonadContrib.Square (
                             -- * Usage
                             -- $usage
                             square ) where

import XMonad
import Graphics.X11.Xlib
import XMonadContrib.LayoutHelpers ( l2lModDo, idModify )

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- >   import XMonadContrib.Square
--
-- An example layout using square together with "XMonadContrib.Combo"
-- to make the very last area square:
--
-- > , combo (combo (mirror $ twoPane 0.03 0.85),1)] (twoPane 0.03 0.5) )
-- >                [(twoPane 0.03 0.2,1),(combo [(twoPane 0.03 0.8,1),(square,1)]
-- >         [(tabbed,3),(tabbed,30),(tabbed,1),(tabbed,1)]

-- %import XMonadContrib.Square

square :: Layout a
square = Layout { doLayout = l2lModDo arrange, modifyLayout = idModify }
 where arrange :: Rectangle -> [a] -> [(a, Rectangle)]
       arrange rect ws@(_:_) = map (\w->(w,rest)) (init ws) ++ [(last ws,sq)]
                 where (rest, sq) = splitSquare rect
       arrange _ [] = []

splitSquare :: Rectangle -> (Rectangle, Rectangle)
splitSquare (Rectangle x y w h)
    | w > h = (Rectangle x y (w - h) h, Rectangle (x+fromIntegral (w-h)) y h h)
    | otherwise = (Rectangle x y w (h-w), Rectangle x (y+fromIntegral (h-w)) w w)
