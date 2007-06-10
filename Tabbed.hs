module XMonadContrib.Tabbed ( tabbed ) where

-- This module defines a tabbed layout.

-- You can use this module with the following in your config file:

-- import XMonadContrib.Tabbed

-- defaultLayouts :: [Layout]
-- defaultLayouts = [ tabbed
--                  , ... ]

import Control.Monad ( forM )
import Control.Monad.State ( gets )

import Graphics.X11.Xlib
import XMonad
import XMonadContrib.Decoration
import Operations ( focus )
import qualified StackSet as W

import XMonadContrib.NamedWindows

tabbed :: Layout
tabbed =  Layout { doLayout = dolay, modifyLayout = const (return Nothing) }

dolay :: Rectangle -> [Window] -> X [(Window, Rectangle)]
dolay sc [w] = return [(w,sc)]
dolay sc@(Rectangle x _ wid _) ws =
    do let ts = gentabs x wid (length ws)
           tws = zip ts ws
           maketab (t,w) = newDecoration t 1 0x000000 0x00FFFF (drawtab t w)  (focus w)
           drawtab r@(Rectangle _ _ wt ht) w d w' gc =
               do nw <- getName w
                  focusw <- gets (W.focus . W.stack . W.workspace . W.current . windowset)
                  let tabcolor = if focusw == w then 0xBBBBBB else 0x888888
                  io $ setForeground d gc tabcolor
                  io $ fillRectangles d w' gc [Rectangle 0 0 wt ht]
                  io $ setForeground d gc 0x000000
                  centerText d w' gc r (show nw)
           centerText d w' gc (Rectangle _ _ wt ht) name =
               do font <- io (fontFromGC d gc >>= queryFont d)
                  -- let (_,namew,nameh,_) = textExtents font name -- textExtents causes a crash!
                  -- let nameh = ht `div` 2
                  --     namew = textWidth font name -- textWidth also causes a crash!
                  let nameh = ht - 6
                      namew = wt - 20
                  io $ drawString d w' gc
                         (fromIntegral (wt `div` 2) - fromIntegral (namew `div` 2))
                         (fromIntegral (ht `div` 2) + fromIntegral (nameh `div` 2)) name
       forM tws maketab
       return [ (w,shrink sc) | w <- ws ]

shrink :: Rectangle -> Rectangle
shrink (Rectangle x y w h) = Rectangle x (y+tabsize) w (h-tabsize)

gentabs :: Position -> Dimension -> Int -> [Rectangle]
gentabs _ _ 0 = []
gentabs x1 w num = Rectangle x1 0 (wid - 2) (tabsize - 2)
                   : gentabs (x1 + fromIntegral wid) (w - wid) (num - 1)
                              where wid = w `div` (fromIntegral num)

tabsize :: Integral a => a
tabsize = 20
