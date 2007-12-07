-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.XUtils
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for painting on the screen
--
-----------------------------------------------------------------------------

module XMonad.Util.XUtils  ( 
                             -- * Usage:
                             -- $usage
                               averagePixels
                             , createNewWindow
                             , showWindow
                             , hideWindow
                             , deleteWindow
                             , paintWindow
                             , paintAndWrite
			     , stringToPixel
                            ) where


import Data.Maybe
import XMonad
import XMonad.Util.Font
import Control.Monad

-- $usage
-- See "XMonad.Layout.Tabbed" or "XMonad.Layout.DragPane" for usage
-- examples

-- | Compute the weighted average the colors of two given Pixel values.
averagePixels :: Pixel -> Pixel -> Double -> X Pixel
averagePixels p1 p2 f =
    do d <- asks display
       let cm = defaultColormap d (defaultScreen d)
       [Color _ r1 g1 b1 _,Color _ r2 g2 b2 _] <- io $ queryColors d cm [Color p1 0 0 0 0,Color p2 0 0 0 0]
       let mn x1 x2 = round (fromIntegral x1 * f + fromIntegral x2 * (1-f))
       Color p _ _ _ _ <- io $ allocColor d cm (Color 0 (mn r1 r2) (mn g1 g2) (mn b1 b2) 0)
       return p
-- | Create a simple window given a rectangle. If Nothing is given
-- only the exposureMask will be set, otherwise the Just value.
-- Use 'showWindow' to map and hideWindow to unmap.
createNewWindow :: Rectangle -> Maybe EventMask -> String -> X Window
createNewWindow (Rectangle x y w h) m col = do
  d   <- asks display
  rw  <- asks theRoot
  c <- stringToPixel d col
  win <- io $ createSimpleWindow d rw x y w h 0 c c
  case m of
    Just em -> io $ selectInput d win em
    Nothing -> io $ selectInput d win exposureMask
  return win

-- | Map a window
showWindow :: Window -> X ()
showWindow w = do
  d <- asks display
  io $ mapWindow d w

-- | unmap a window
hideWindow :: Window -> X ()
hideWindow w = do
  d <- asks display
  io $ unmapWindow d w

-- | destroy a window
deleteWindow :: Window -> X ()
deleteWindow w = do
  d <- asks display
  io $ destroyWindow d w

-- | Fill a window with a rectangle and a border
paintWindow :: Window     -- ^ The window where to draw 
            -> Dimension  -- ^ Window width
            -> Dimension  -- ^ Window height 
            -> Dimension  -- ^ Border width
            -> String     -- ^ Window background color
            -> String     -- ^ Border color
            -> X ()
paintWindow w wh ht bw c bc =
    paintWindow' w (Rectangle 0 0 wh ht) bw c bc Nothing

-- | Fill a window with a rectangle and a border, and write a string at given position
paintAndWrite :: Window     -- ^ The window where to draw 
              -> XMonadFont -- ^ XMonad Font for drawing
              -> Dimension  -- ^ Window width
              -> Dimension  -- ^ Window height 
              -> Dimension  -- ^ Border width
              -> String     -- ^ Window background color
              -> String     -- ^ Border color
              -> String     -- ^ String color
              -> String     -- ^ String background color
              -> Align      -- ^ String 'Align'ment
              -> String     -- ^ String to be printed
              -> X ()
paintAndWrite w fs wh ht bw bc borc ffc fbc al str = do
    (x,y) <- stringPosition fs (Rectangle 0 0 wh ht) al str
    paintWindow' w (Rectangle x y wh ht) bw bc borc ms
    where ms    = Just (fs,ffc,fbc,str)

-- This stuff is not exported

paintWindow' :: Window -> Rectangle -> Dimension -> String -> String -> Maybe (XMonadFont,String,String,String) -> X ()
paintWindow' win (Rectangle x y wh ht) bw color b_color str = do
  d  <- asks display
  p  <- io $ createPixmap d win wh ht (defaultDepthOfScreen $ defaultScreenOfDisplay d)
  gc <- io $ createGC d p
  -- draw
  io $ setGraphicsExposures d gc False
  [color',b_color'] <- mapM (stringToPixel d) [color,b_color]
  -- we start with the border
  io $ setForeground d gc b_color'
  io $ fillRectangle d p gc 0 0 wh ht
  -- and now again
  io $ setForeground d gc color'
  io $ fillRectangle d p gc (fi bw) (fi bw) ((wh - (bw * 2))) (ht - (bw * 2))
  when (isJust str) $ do
    let (xmf,fc,bc,s) = fromJust str
    printStringXMF d p xmf gc fc bc x y s
  -- copy the pixmap over the window
  io $ copyArea      d p win gc 0 0 wh ht 0 0
  -- free the pixmap and GC
  io $ freePixmap    d p
  io $ freeGC        d gc

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
