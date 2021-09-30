{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DragPane
-- Description :  Split the screen either horizontally or vertically and show two windows.
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
--                    David Roundy <droundy@darcs.net>,
--                    Andrea Rossato <andrea.rossato@unibz.it>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layouts that splits the screen either horizontally or vertically and
-- shows two windows.  The first window is always the master window, and
-- the other is either the currently focused window or the second window in
-- layout order.

-----------------------------------------------------------------------------

module XMonad.Layout.DragPane (
                               -- * Usage
                               -- $usage
                                dragPane
                              , DragPane, DragType (..)
                              ) where

import XMonad
import Data.Unique

import qualified XMonad.StackSet as W
import XMonad.Util.Invisible
import XMonad.Util.XUtils

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DragPane
--
-- Then edit your @layoutHook@ by adding the DragPane layout:
--
-- > myLayout = dragPane Horizontal 0.1 0.5 ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

halfHandleWidth :: Integral a => a
halfHandleWidth = 1

handleColor :: String
handleColor = "#000000"

dragPane :: DragType -> Double -> Double -> DragPane a
dragPane = DragPane (I Nothing)

data DragPane a =
    DragPane (Invisible Maybe (Window,Rectangle,Int)) DragType Double Double
             deriving ( Show, Read )

data DragType = Horizontal | Vertical deriving ( Show, Read )

instance LayoutClass DragPane a where
    doLayout d@(DragPane _ Vertical   _ _) = doLay id d
    doLayout d@(DragPane _ Horizontal _ _) = doLay mirrorRect d
    handleMessage = handleMess

data SetFrac = SetFrac Int Double deriving ( Show, Read, Eq)
instance Message SetFrac

handleMess :: DragPane a -> SomeMessage -> X (Maybe (DragPane a))
handleMess d@(DragPane mb@(I (Just (win,_,ident))) ty delta split) x
    | Just e <- fromMessage x :: Maybe Event = do handleEvent d e
                                                  return Nothing
    | Just Hide             <- fromMessage x = do hideWindow win
                                                  return $ Just (DragPane mb ty delta split)
    | Just ReleaseResources <- fromMessage x = do deleteWindow win
                                                  return $ Just (DragPane (I Nothing) ty delta split)
    -- layout specific messages
    | Just Shrink <- fromMessage x = return $ Just (DragPane mb ty delta (split - delta))
    | Just Expand <- fromMessage x = return $ Just (DragPane mb ty delta (split + delta))
    | Just (SetFrac ident' frac) <- fromMessage x, ident' == ident =
                                     return $ Just (DragPane mb ty delta frac)
handleMess _ _ = return Nothing

handleEvent :: DragPane a -> Event -> X ()
handleEvent (DragPane (I (Just (win,r,ident))) ty _ _)
            ButtonEvent{ev_window = thisw, ev_subwindow = thisbw, ev_event_type = t }
    | t == buttonPress && thisw == win || thisbw == win  =
  mouseDrag (\ex ey -> do
             let frac = case ty of
                        Vertical   -> (fromIntegral ex - fromIntegral (rect_x r))/fromIntegral (rect_width  r)
                        Horizontal -> (fromIntegral ey - fromIntegral (rect_x r))/fromIntegral (rect_width r)
             sendMessage (SetFrac ident frac))
            (return ())
handleEvent _ _  = return ()

doLay :: (Rectangle -> Rectangle) -> DragPane a -> Rectangle -> W.Stack a -> X ([(a, Rectangle)], Maybe (DragPane a))
doLay mirror (DragPane mw ty delta split) r s = do
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
            I (Just (w,_,ident)) -> do
                    w' <- deleteWindow w >> newDragWin handr
                    return (wrs, Just $ DragPane (I $ Just (w',r',ident)) ty delta split)
            I Nothing -> do
                    w <- newDragWin handr
                    i <- io newUnique
                    return (wrs, Just $ DragPane (I $ Just (w,r',hashUnique i)) ty delta split)
     else return (wrs, Nothing)


newDragWin :: Rectangle -> X Window
newDragWin r = do
  let mask = Just $ exposureMask .|. buttonPressMask
  w <- createNewWindow r mask handleColor False
  showWindow  w
  d <- asks display
  liftIO $ lowerWindow d w
  return      w
