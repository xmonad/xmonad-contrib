-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Tabbed
-- Copyright   :  (c) David Roundy
-- License     :  ??? GPL 2 ???
-- 
-- Maintainer  :  email@address.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module XMonadContrib.Tabbed ( 
                             -- * Usage:
                             -- $usage
                             tabbed
                            ) where

import Control.Monad ( forM, liftM )
import Control.Monad.State ( gets )

import Graphics.X11.Xlib
import XMonad
import XMonadContrib.Decoration
import Operations ( focus )
import qualified StackSet as W

import XMonadContrib.NamedWindows

-- $usage
-- You can use this module with the following in your configuration file:
--
-- > import XMonadContrib.Tabbed
--
-- > defaultLayouts :: [Layout]
-- > defaultLayouts = [ tabbed
-- >                 , ... ]


tabbed :: Layout
tabbed =  Layout { doLayout = dolay, modifyLayout = const (return Nothing) }

dolay :: Rectangle -> W.Stack Window -> X [(Window, Rectangle)]
dolay sc (W.Stack w [] []) = return [(w,sc)]
dolay sc@(Rectangle x y wid _) s@(W.Stack w _ _) =
    do let ws = W.integrate s
           ts = gentabs x y wid (length ws)
           tws = zip ts ws
           maketab (t,w) = newDecoration w t 1 0x000000 0x777777 (drawtab t w) (focus w)
           drawtab r@(Rectangle _ _ wt ht) w d w' gc =
               do nw <- getName w
                  tabcolor <- (maybe 0x888888 (\focusw -> if focusw == w then 0xBBBBBB else 0x888888) . W.peek) `liftM` gets windowset
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
                      namew = wt - 10
                  io $ drawString d w' gc
                         (fromIntegral (wt `div` 2) - fromIntegral (namew `div` 2))
                         (fromIntegral (ht `div` 2) + fromIntegral (nameh `div` 2)) name
       forM tws maketab
       return [ (w,shrink sc) ]

shrink :: Rectangle -> Rectangle
shrink (Rectangle x y w h) = Rectangle x (y+tabsize) w (h-tabsize)

gentabs :: Position -> Position -> Dimension -> Int -> [Rectangle]
gentabs _ _ _ 0 = []
gentabs x y w num = Rectangle x y (wid - 2) (tabsize - 2)
                   : gentabs (x + fromIntegral wid) y (w - wid) (num - 1)
                              where wid = w `div` (fromIntegral num)

tabsize :: Integral a => a
tabsize = 20
