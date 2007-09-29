-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.XUtils
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for painting on the screem
--
-----------------------------------------------------------------------------

module XMonadContrib.XUtils  ( 
                             -- * Usage:
                             -- $usage
                             stringToPixel
                             , initFont
                             , createNewWindow
                             , showWindow
                             , hideWindow
                             , deleteWindow
                             , paintWindow
                             , paintAndWrite
                            ) where


import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Control.Monad.Reader
import Data.Maybe
import XMonad
import Operations

-- $usage
-- See Tabbed or DragPane for usage examples

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
stringToPixel :: String -> X Pixel
stringToPixel s = do
  d <- asks display
  return =<< io $ catch (getIt d) (fallBack d)
    where getIt    d = initColor d s
          fallBack d = const $ return $ blackPixel d (defaultScreen d)

-- | Given a fontname returns the fonstructure. If the font name is
--  not valid the default font will be loaded and returned.
initFont :: String -> X FontStruct
initFont s = do
  d <- asks display
  return =<< io $ catch (getIt d) (fallBack d)
      where getIt    d = loadQueryFont d s
            fallBack d = const $ loadQueryFont d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- | Create a simple window given a rectangle. If Nothing is given
-- only the exposureMask will be set, otherwise the Just value.
-- Use 'showWindow' to map and hideWindow to unmap.
createNewWindow :: Rectangle -> Maybe EventMask -> X Window
createNewWindow (Rectangle x y w h) m = do
  d   <- asks display
  rw  <- asks theRoot
  win <- io $ createSimpleWindow d rw x y w h 0 0 0
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
              -> FontStruct -- ^ The FontStruct
              -> Dimension  -- ^ Window width
              -> Dimension  -- ^ Window height 
              -> Dimension  -- ^ Border width
              -> String     -- ^ Window background color
              -> String     -- ^ Border color
              -> Position   -- ^ String x position
              -> Position   -- ^ String y position
              -> String     -- ^ String color
              -> String     -- ^ String background color
              -> String     -- ^ String to be printed
              -> X ()
paintAndWrite w fs wh ht bw bc borc x y ffc fbc str =
    paintWindow' w (Rectangle x y wh ht) bw bc borc (Just (fs,ffc,fbc,str))

-- This stuf is not exported

paintWindow' :: Window -> Rectangle -> Dimension -> String -> String -> Maybe (FontStruct,String,String,String) -> X ()
paintWindow' win (Rectangle x y wh ht) bw color b_color str = do
  d  <- asks display
  p  <- io $ createPixmap d win wh ht (defaultDepthOfScreen $ defaultScreenOfDisplay d)
  gc <- io $ createGC d p
  let fi = fromIntegral
  -- draw
  io $ setGraphicsExposures d gc False
  [c',bc'] <- mapM stringToPixel [color,b_color]
  -- we start with the border
  io $ setForeground d gc bc'
  io $ fillRectangle d p gc 0 0 wh ht
  -- and now again
  io $ setForeground d gc c'
  io $ fillRectangle d p gc (fi bw) (fi bw) ((wh - (bw * 2))) (ht - (bw * 2))
  when (isJust str) $ do
    let (fs,fc,bc,s) = fromJust str
    io $ setFont d gc $ fontFromFontStruct fs
    printString d p gc fc bc x y s
  -- copy the pixmap over the wind
  io $ copyArea      d p win gc 0 0 wh ht 0 0
  -- free the pixmap and GC
  io $ freePixmap    d p
  io $ freeGC        d gc

-- | Prints a string on a 'Drawable'
printString :: Display -> Drawable -> GC -> String -> String
            -> Position -> Position -> String  -> X ()
printString d drw gc fc bc x y s = do
  [fc',bc'] <- mapM stringToPixel [fc,bc]
  io $ setForeground   d gc fc'
  io $ setBackground   d gc bc'
  io $ drawImageString d drw gc x y s
