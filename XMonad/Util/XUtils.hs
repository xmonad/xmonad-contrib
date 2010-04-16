-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.XUtils
-- Copyright   :  (c) 2007 Andrea Rossato
--                    2010 Alejandro Serrano
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for painting on the screen
--
-----------------------------------------------------------------------------

module XMonad.Util.XUtils
    ( -- * Usage:
      -- $usage
      averagePixels
    , createNewWindow
    , showWindow
    , showWindows
    , hideWindow
    , hideWindows
    , deleteWindow
    , deleteWindows
    , paintWindow
    , paintAndWrite
    , paintTextAndIcons
    , stringToPixel
    , fi
    ) where

import Data.Maybe
import XMonad
import XMonad.Util.Font
import XMonad.Util.Image
import Control.Monad

-- $usage
-- See "XMonad.Layout.Tabbed" or "XMonad.Layout.DragPane" or 
-- "XMonad.Layout.Decoration" for usage examples

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
createNewWindow :: Rectangle -> Maybe EventMask -> String -> Bool -> X Window
createNewWindow (Rectangle x y w h) m col o = do
  d   <- asks display
  rw  <- asks theRoot
  c   <- stringToPixel d col
  win <- io $ mkWindow d (defaultScreenOfDisplay d) rw x y w h c o
  case m of
    Just em -> io $ selectInput d win em
    Nothing -> io $ selectInput d win exposureMask
  return win

-- | Map a window
showWindow :: Window -> X ()
showWindow w = do
  d <- asks display
  io $ mapWindow d w

-- | the list version
showWindows :: [Window] -> X ()
showWindows = mapM_ showWindow

-- | unmap a window
hideWindow :: Window -> X ()
hideWindow w = do
  d <- asks display
  io $ unmapWindow d w

-- | the list version
hideWindows :: [Window] -> X ()
hideWindows = mapM_ hideWindow

-- | destroy a window
deleteWindow :: Window -> X ()
deleteWindow w = do
  d <- asks display
  io $ destroyWindow d w

-- | the list version
deleteWindows :: [Window] -> X ()
deleteWindows = mapM_ deleteWindow

-- | Fill a window with a rectangle and a border
paintWindow :: Window     -- ^ The window where to draw
            -> Dimension  -- ^ Window width
            -> Dimension  -- ^ Window height
            -> Dimension  -- ^ Border width
            -> String     -- ^ Window background color
            -> String     -- ^ Border color
            -> X ()
paintWindow w wh ht bw c bc =
    paintWindow' w (Rectangle 0 0 wh ht) bw c bc Nothing Nothing

-- | Fill a window with a rectangle and a border, and write
-- | a number of strings to given positions
paintAndWrite :: Window     -- ^ The window where to draw
              -> XMonadFont -- ^ XMonad Font for drawing
              -> Dimension  -- ^ Window width
              -> Dimension  -- ^ Window height
              -> Dimension  -- ^ Border width
              -> String     -- ^ Window background color
              -> String     -- ^ Border color
              -> String     -- ^ String color
              -> String     -- ^ String background color
              -> [Align]    -- ^ String 'Align'ments
              -> [String]   -- ^ Strings to be printed
              -> X ()
paintAndWrite w fs wh ht bw bc borc ffc fbc als strs = do
    d <- asks display
    strPositions <- forM (zip als strs) $ \(al, str) ->
        stringPosition d fs (Rectangle 0 0 wh ht) al str
    let ms = Just (fs,ffc,fbc, zip strs strPositions)
    paintWindow' w (Rectangle 0 0 wh ht) bw bc borc ms Nothing

-- | Fill a window with a rectangle and a border, and write
-- | a number of strings and a number of icons to given positions
paintTextAndIcons :: Window      -- ^ The window where to draw
                  -> XMonadFont  -- ^ XMonad Font for drawing
                  -> Dimension   -- ^ Window width
                  -> Dimension   -- ^ Window height
                  -> Dimension   -- ^ Border width
                  -> String      -- ^ Window background color
                  -> String      -- ^ Border color
                  -> String      -- ^ String color
                  -> String      -- ^ String background color
                  -> [Align]     -- ^ String 'Align'ments
                  -> [String]    -- ^ Strings to be printed
                  -> [Placement] -- ^ Icon 'Placements'
                  -> [[[Bool]]]  -- ^ Icons to be printed
                  -> X ()
paintTextAndIcons w fs wh ht bw bc borc ffc fbc als strs i_als icons = do
    d <- asks display
    strPositions <- forM (zip als strs) $ \(al, str) ->
        stringPosition d fs (Rectangle 0 0 wh ht) al str
    let iconPositions = map ( \(al, icon) -> iconPosition (Rectangle 0 0 wh ht) al icon ) (zip i_als icons)
        ms = Just (fs,ffc,fbc, zip strs strPositions)
        is = Just (ffc, fbc, zip iconPositions icons)
    paintWindow' w (Rectangle 0 0 wh ht) bw bc borc ms is

-- This stuff is not exported

-- | Paints a titlebar with some strings and icons
-- drawn inside it.
-- Not exported.
paintWindow' :: Window -> Rectangle -> Dimension -> String -> String
                -> Maybe (XMonadFont,String,String,[(String, (Position, Position))]) 
                -> Maybe (String, String, [((Position, Position), [[Bool]])]) -> X ()
paintWindow' win (Rectangle _ _ wh ht) bw color b_color strStuff iconStuff = do
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
  io $ fillRectangle d p gc (fi bw) (fi bw) (wh - (bw * 2)) (ht - (bw * 2))
  -- paint strings
  when (isJust strStuff) $ do
    let (xmf,fc,bc,strAndPos) = fromJust strStuff
    forM_ strAndPos $ \(s, (x, y)) ->
        printStringXMF d p xmf gc fc bc x y s
  -- paint icons
  when (isJust iconStuff) $ do
    let (fc, bc, iconAndPos) = fromJust iconStuff
    forM_ iconAndPos $ \((x, y), icon) ->
      drawIcon d p gc fc bc x y icon
  -- copy the pixmap over the window
  io $ copyArea      d p win gc 0 0 wh ht 0 0
  -- free the pixmap and GC
  io $ freePixmap    d p
  io $ freeGC        d gc

-- | Creates a window with the possibility of setting some attributes.
-- Not exported.
mkWindow :: Display -> Screen -> Window -> Position
         -> Position -> Dimension -> Dimension -> Pixel -> Bool -> IO Window
mkWindow d s rw x y w h p o = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect .|. cWBackPixel .|. cWBorderPixel
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes o
           set_border_pixel      attributes p
           set_background_pixel  attributes p
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes

