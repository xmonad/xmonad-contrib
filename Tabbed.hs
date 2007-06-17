-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Tabbed
-- Copyright   :  (c) David Roundy
-- License     :  BSD-style (see xmonad/LICENSE)
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
import Operations ( focus, initColor )
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
dolay sc@(Rectangle x y wid _) s@(W.Stack w _ _) = withDisplay $ \dpy ->
    do activecolor   <- io $ initColor dpy "#BBBBBB"
       inactivecolor <- io $ initColor dpy "#888888"
       textcolor     <- io $ initColor dpy "#000000"
       bgcolor       <- io $ initColor dpy "#000000"
       let ws = W.integrate s
           ts = gentabs x y wid (length ws)
           tws = zip ts ws
           maketab (t,ow) = newDecoration ow t 1 bgcolor activecolor (drawtab t ow) (focus ow)
           drawtab r@(Rectangle _ _ wt ht) ow d w' gc =
               do nw <- getName ow
                  tabcolor <- (maybe inactivecolor (\focusw -> if focusw == ow then activecolor else inactivecolor) . W.peek) `liftM` gets windowset
                  io $ setForeground d gc tabcolor
                  io $ fillRectangles d w' gc [Rectangle 0 0 wt ht]
                  io $ setForeground d gc textcolor
                  centerText d w' gc r (show nw)
           centerText d w' gc (Rectangle _ _ wt ht) name =
               do fontst <- io $ loadQueryFont d "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*"
                  io $ setFont d gc (fontFromFontStruct fontst)
                  let (_,asc,_,_) = textExtents fontst name
                      width = textWidth fontst name
                  io $ drawString d w' gc
                         (fromIntegral (wt `div` 2) - fromIntegral (width `div` 2))
                         (fromIntegral ht - fromIntegral (asc `div` 2)) name
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
