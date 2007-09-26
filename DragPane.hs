{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DragPane
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
--                    David Roundy <droundy@darcs.net>,
--                    Andrea Rossato <andrea.rossato@unibz.it>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
--                Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layouts that splits the screen either horizontally or vertically and
-- shows two windows.  The first window is always the master window, and
-- the other is either the currently focused window or the second window in
-- layout order.

-----------------------------------------------------------------------------

module XMonadContrib.DragPane (
                               -- * Usage
                               -- $usage
                                DragPane (DragPane)
                              , DragType (..)
                              ) where

import Control.Monad.Reader ( asks )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XMonad
import Data.Bits
import Data.Unique

import Operations 
import qualified StackSet as W 

-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.DragPane
--
--  and add, to the list of layouts:
--
-- > DragPane Nothing Vertical 0.1 0.5

halfHandleWidth :: Integral a => a
halfHandleWidth = 1

handleColor :: String
handleColor = "#000000"

data DragPane a = 
    DragPane (Maybe (Window,Rectangle,Int)) DragType Double Double 
             deriving ( Show, Read )

data DragType = Horizontal | Vertical deriving ( Show, Read )

instance Layout DragPane Window where
    doLayout d@(DragPane _ Vertical _ _) = doLay id d
    doLayout d@(DragPane _ Horizontal _ _) = doLay mirrorRect d
    handleMessage = handleMess

data SetFrac = SetFrac Int Double deriving ( Show, Read, Eq, Typeable )
instance Message SetFrac

handleMess :: DragPane Window -> SomeMessage -> X (Maybe (DragPane Window)) 
handleMess d@(DragPane mb@(Just (win,_,ident)) ty delta split) x
    | Just e <- fromMessage x :: Maybe Event = do
  handleEvent d e 
  return Nothing
    | Just Hide             <- fromMessage x = do
  hideDragWin win
  return $ Just (DragPane mb ty delta split)
    | Just ReleaseResources <- fromMessage x = do
  destroyDragWin win
  return $ Just (DragPane Nothing ty delta split)
    -- layout specific messages
    | Just Shrink <- fromMessage x = return $ Just (DragPane mb ty delta (split - delta))
    | Just Expand <- fromMessage x = return $ Just (DragPane mb ty delta (split + delta))
    | Just (SetFrac ident' frac) <- fromMessage x, ident' == ident = do
                                     return $ Just (DragPane mb ty delta frac)
handleMess _ _ = return Nothing

handleEvent :: DragPane Window -> Event -> X ()
handleEvent (DragPane (Just (win,r,ident)) ty _ _) 
            (ButtonEvent {ev_window = thisw, ev_subwindow = thisbw, ev_event_type = t })
    | t == buttonPress && thisw == win || thisbw == win  = do
  mouseDrag (\ex ey -> do
             let frac = case ty of
                          Vertical   -> (fromIntegral ex - (fromIntegral $ rect_x r))/(fromIntegral $ rect_width  r)
                          Horizontal -> (fromIntegral ey - (fromIntegral $ rect_y r))/(fromIntegral $ rect_height r)
             sendMessage (SetFrac ident frac))
            (return ())
handleEvent _ _ = return ()

doLay :: (Rectangle -> Rectangle) -> DragPane Window -> Rectangle -> W.Stack a -> X ([(a, Rectangle)], Maybe (DragPane a))
doLay mirror (DragPane mw ty delta split) r s = do
  handlec <- withDisplay $ \dpy -> io $ initColor dpy handleColor
  let r' = mirror r
      (left', right') = splitHorizontallyBy split r'
      left = case left' of Rectangle x y w h ->
                               mirror $ Rectangle x y (w-halfHandleWidth) h
      right = case right' of
                Rectangle x y w h ->
		    mirror $ Rectangle (x+halfHandleWidth) y (w-halfHandleWidth) h
      handr = case left' of
                Rectangle x y w h ->
                    mirror $ Rectangle (x + fromIntegral w - halfHandleWidth) y (2*halfHandleWidth) h
      wrs = case reverse (W.up s) of
              (master:_) -> [(master,left),(W.focus s,right)]
              [] -> case W.down s of
                      (next:_) -> [(W.focus s,left),(next,right)]
                      [] -> [(W.focus s, r)]
  if length wrs > 1 
     then case mw of
            Just (w,_,ident) -> do 
                    w' <- updateDragWin w handlec handr
                    return (wrs, Just $ DragPane (Just (w',r',ident)) ty delta split)
            Nothing -> do 
                    w <- newDragWin handlec handr
                    i <- io $ newUnique
                    return (wrs, Just $ DragPane (Just (w,r',hashUnique i)) ty delta split)
     else return (wrs, Nothing)


newDragWin :: Pixel -> Rectangle -> X Window
newDragWin p r = do
  d  <- asks display
  dragWin d p r

updateDragWin :: Window -> Pixel -> Rectangle -> X Window
updateDragWin w p r = do
    d  <- asks display
    io $ destroyWindow d w
    dragWin d p r

hideDragWin :: Window -> X ()
hideDragWin w = do
    d  <- asks display
    io $ unmapWindow d w

destroyDragWin :: Window -> X ()
destroyDragWin w = do
    d  <- asks display
    io $ destroyWindow d w

dragWin :: Display -> Pixel -> Rectangle -> X Window
dragWin d p (Rectangle x y wt ht) = do
  rt <- asks theRoot
  w  <- io $ createSimpleWindow d rt x y wt ht 0 p p
  io $ selectInput d w $ exposureMask .|. buttonPressMask
  io $ mapWindow d w
  return w
