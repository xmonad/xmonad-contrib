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
                            , Shrinker, shrinkText
                            , TConf (..), defaultTConf
                            ) where

import Control.Monad.State ( gets )

import Graphics.X11.Xlib
import XMonad
import XMonadContrib.Decoration
import Operations ( focus, initColor )
import qualified StackSet as W

import XMonadContrib.NamedWindows
import XMonadContrib.SimpleStacking ( simpleStacking )
import XMonadContrib.LayoutHelpers ( idModify )

-- $usage
-- You can use this module with the following in your configuration file:
--
-- > import XMonadContrib.Tabbed
--
-- > defaultLayouts :: [Layout Window]
-- > defaultLayouts = [ tabbed shrinkText defaultTConf
-- >                  , ... ]
--
-- You can also edit the default configuration options.
--
-- > myconfig = defaultTConf { inactiveBolderColor = "#FF0000"
-- >                         , activeTextColor = "#00FF00"}
--
-- and
--
-- > defaultLayouts = [ tabbed shrinkText myconfig
-- >                  , ... ]

data TConf = 
    TConf { activeColor :: String
          , inactiveColor :: String
          , activeBorderColor :: String
          , inactiveTextColor :: String
          , inactiveBorderColor :: String
          , activeTextColor :: String
          , fontName :: String
          , tabSize :: Int
          } deriving (Show, Read)

defaultTConf :: TConf
defaultTConf = 
    TConf { activeColor ="#999999"
          , inactiveColor = "#666666"
          , activeBorderColor = "#FFFFFF"
          , inactiveBorderColor = "#BBBBBB"
          , activeTextColor = "#FFFFFF"
          , inactiveTextColor = "#BFBFBF"
          , fontName = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
          , tabSize = 20
          }

tabbed :: Shrinker -> TConf -> Layout Window
tabbed s t = simpleStacking $ tabbed' s t

tabbed' :: Shrinker -> TConf -> Layout Window
tabbed' shrinkT config =  Layout { doLayout = dolay shrinkT config, modifyLayout = idModify }

dolay :: Shrinker -> TConf
      -> Rectangle -> W.Stack Window -> X ([(Window, Rectangle)], Maybe (Layout Window))
dolay _ _ sc (W.Stack w [] []) = return ([(w,sc)], Nothing)
dolay shr conf sc@(Rectangle x y wid _) s = withDisplay $ \dpy ->
    do ac   <- io $ initColor dpy $ activeColor conf
       ic <- io $ initColor dpy $ inactiveColor conf
       abc <- io $ initColor dpy $ activeBorderColor conf
       ibc <- io $ initColor dpy $ inactiveBorderColor conf
       atc <- io $ initColor dpy $ activeTextColor conf 
       itc <- io $ initColor dpy $ inactiveTextColor conf
       let ws = W.integrate s
           ts = gentabs conf x y wid (length ws)
           tws = zip ts ws
           focusColor w incol actcol = (maybe incol (\focusw -> if focusw == w 
                                                                then actcol else incol) . W.peek) 
                                       `fmap` gets windowset
           make_tabs [] l = return l
           make_tabs (tw':tws') l = do bc <- focusColor (snd tw') ibc abc
                                       l' <- maketab tw' bc l
                                       make_tabs tws' l'
           maketab (t,ow) bg = newDecoration ow t 1 bg ac
                                (fontName conf) (drawtab t ow) (focus ow)
           drawtab r@(Rectangle _ _ wt ht) ow d w' gc fn =
               do nw <- getName ow
                  (fc,tc) <- focusColor ow (ic,itc) (ac,atc)
                  io $ setForeground d gc fc
                  io $ fillRectangles d w' gc [Rectangle 0 0 wt ht]
                  io $ setForeground d gc tc
                  centerText d w' gc fn r (show nw)
           centerText d w' gc fontst (Rectangle _ _ wt ht) name =
               do let (_,asc,_,_) = textExtents fontst name
                      name' = shrinkWhile shr (\n -> textWidth fontst n >
                                                     fromIntegral wt - fromIntegral (ht `div` 2)) name
                      width = textWidth fontst name'
                  io $ drawString d w' gc
                         (fromIntegral (wt `div` 2) - fromIntegral (width `div` 2))
                         ((fromIntegral ht + fromIntegral asc) `div` 2) name'
       l' <- make_tabs tws $ tabbed shr conf
       return (map (\w -> (w,shrink conf sc)) ws, Just l')

type Shrinker = String -> [String]

shrinkWhile :: Shrinker -> (String -> Bool) -> String -> String
shrinkWhile sh p x = sw $ sh x
    where sw [n] = n
          sw [] = ""
          sw (n:ns) | p n = sw ns
                    | otherwise = n

shrinkText :: Shrinker
shrinkText "" = [""]
shrinkText cs = cs : shrinkText (init cs)

shrink :: TConf -> Rectangle -> Rectangle
shrink c (Rectangle x y w h) = Rectangle x (y + fromIntegral (tabSize c)) w (h - fromIntegral (tabSize c))

gentabs :: TConf -> Position -> Position -> Dimension -> Int -> [Rectangle]
gentabs _ _ _ _ 0 = []
gentabs c x y w num = Rectangle x y (wid - 2) (fromIntegral (tabSize c) - 2)
                      : gentabs c (x + fromIntegral wid) y (w - wid) (num - 1)
                          where wid = w `div` (fromIntegral num)
