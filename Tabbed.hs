module XMonadContrib.Tabbed ( tabbed ) where

-- This module defines a tabbed layout.

-- You can use this module with the following in your config file:

-- import XMonadContrib.Tabbed

-- defaultLayouts :: [Layout]
-- defaultLayouts = [ tabbed
--                  , ... ]

import Control.Monad ( forM )

import Graphics.X11.Xlib
import XMonad
import XMonadContrib.Decoration
import Operations ( focus )

tabbed :: Layout
tabbed =  Layout { doLayout = dolay, modifyLayout = const (return Nothing) }

dolay :: Rectangle -> [Window] -> X [(Window, Rectangle)]
dolay sc [w] = return [(w,sc)]
dolay sc@(Rectangle x _ wid _) ws =
    do let ts = gentabs x wid (length ws)
           tws = zip ts ws
       forM tws $ \(t,w) -> newDecoration t 1 0xFF0000 0x00FFFF (trace "draw") (focus w)
       return [ (w,shrink sc) | w <- ws ]

shrink :: Rectangle -> Rectangle
shrink (Rectangle x y w h) = Rectangle x (y+tabsize) w (h-tabsize)

gentabs :: Position -> Dimension -> Int -> [Rectangle]
gentabs _ _ 0 = []
gentabs x1 w num = Rectangle x1 0 (wid - 2) (tabsize - 2)
                   : gentabs (x1 + fromIntegral wid) (w - wid) (num - 1)
                              where wid = w `div` (fromIntegral num)

tabsize :: Integral a => a
tabsize = 30
