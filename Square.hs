-- A layout that splits the screen into a square area and the rest of the
-- screen.


-- An example layout using square to make the very last area square:

--  , combo [(tabbed,3),(tabbed,30),(tabbed,1),(tabbed,1)]
--              (combo [(twoPane 0.03 0.2,1)
--                     ,(combo [(twoPane 0.03 0.8,1),(square,1)]
--                                 (mirror $ twoPane 0.03 0.85),1)] (twoPane 0.03 0.5) )

module XMonadContrib.Square ( square ) where

import XMonad
import Graphics.X11.Xlib

square :: Layout
square = Layout { doLayout = arrange, modifyLayout = message }
 where
    arrange rect ws@(_:_) = do
        let (rest, sq) = splitSquare rect
        return (map (\w->(w,rest)) (init ws) ++ [(last ws,sq)])
    arrange _ [] = return []

    message _ = return Nothing

splitSquare :: Rectangle -> (Rectangle, Rectangle)
splitSquare (Rectangle x y w h)
    | w > h = (Rectangle x y (w - h) h, Rectangle (x+fromIntegral (w-h)) y h h)
    | otherwise = (Rectangle x y w (h-w), Rectangle x (y+fromIntegral (h-w)) w w)
