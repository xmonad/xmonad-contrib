-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DragPane
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
--                    David Roundy <droundy@darcs.net>,
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable

-- Layouts that splits the screen either horizontally or vertically and
-- shows two windows.  The first window is always the master window, and
-- the other is either the currently focused window or the second window in
-- layout order.

-----------------------------------------------------------------------------

module XMonadContrib.DragPane (
                               -- * Usage
                               -- $usage
                               dragPane, dragUpDownPane
                              ) where

import Control.Monad.Reader ( asks )
import Graphics.X11.Xlib ( Rectangle( Rectangle ) )
import XMonad
import XMonadContrib.Decoration ( newDecoration )
import Operations ( Resize(..), splitHorizontallyBy, splitVerticallyBy, initColor, mouseDrag, sendMessage )
import StackSet ( focus, up, down)

-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.DragPane
--
--  and add, to the list of layouts:
--
-- > dragPane "" (fromRational delta) (fromRational delta)

halfHandleWidth :: Integral a => a
halfHandleWidth = 2

handleColor :: String
handleColor = "#000000"

dragPane :: String -> Double -> Double -> Layout a
dragPane ident delta split = Layout { doLayout = dolay, modifyLayout = return . message }
 where
    dolay r s = do handlec <- withDisplay $ \dpy -> io $ initColor dpy handleColor
                   root <- asks theRoot
                   let (left', right') = splitHorizontallyBy split r
                       leftmost = fromIntegral $ case r of Rectangle x _ _ _ -> x
                       widt = fromIntegral $ case r of Rectangle _ _ w _ -> w
                       left = case left' of Rectangle x y w h -> Rectangle x y (w-halfHandleWidth) h
                       right = case right' of
                               Rectangle x y w h -> Rectangle (x+halfHandleWidth) y (w-halfHandleWidth) h
                       handr = case left' of
                               Rectangle x y w h ->
                                 Rectangle (x + fromIntegral w - halfHandleWidth) y (2*halfHandleWidth) h
                       wrs = case reverse (up s) of
                             (master:_) -> [(master,left),(focus s,right)]
                             [] -> case down s of
                                   (next:_) -> [(focus s,left),(next,right)]
                                   [] -> [(focus s, r)]
                       handle = newDecoration root handr 0 handlec handlec
                                "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                                (const $ const $ const $ const $ return ()) (doclick)
                       doclick = mouseDrag (\ex _ ->
                                        sendMessage (SetFrac ident ((fromIntegral ex - leftmost)/widt)))
                                        (return ())
                                        
                   ml' <- if length wrs > 1 then Just `fmap` handle (dragPane ident delta split)
                                            else return Nothing
                   return (wrs, ml')
    message x | Just Shrink <- fromMessage x = Just (dragPane ident delta (split - delta))
              | Just Expand <- fromMessage x = Just (dragPane ident delta (split + delta))
              | Just (SetFrac ident' frac) <- fromMessage x, ident' == ident =
                                              Just (dragPane ident delta frac)
    message _ = Nothing

dragUpDownPane :: String -> Double -> Double -> Layout a
dragUpDownPane ident delta split = Layout { doLayout = dolay, modifyLayout = return . message }
 where
    dolay r s = do handlec <- withDisplay $ \dpy -> io $ initColor dpy handleColor
                   root <- asks theRoot
                   let (left', right') = splitVerticallyBy split r
                       leftmost = fromIntegral $ case r of Rectangle _ x _ _ -> x
                       widt = fromIntegral $ case r of Rectangle _ _ _ w -> w
                       left = case left' of Rectangle x y w h -> Rectangle x y w (h-halfHandleWidth)
                       right = case right' of
                               Rectangle x y w h -> Rectangle x (y+halfHandleWidth) w (h-halfHandleWidth)
                       handr = case left' of
                               Rectangle x y w h ->
                                 Rectangle x (y + fromIntegral h - halfHandleWidth) w (2*halfHandleWidth)
                       wrs = case reverse (up s) of
                             (master:_) -> [(master,left),(focus s,right)]
                             [] -> case down s of
                                   (next:_) -> [(focus s,left),(next,right)]
                                   [] -> [(focus s, r)]
                       handle = newDecoration root handr 0 handlec handlec
                                "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                                (const $ const $ const $ const $ return ()) (doclick)
                       doclick = mouseDrag (\_ ey ->
                                        sendMessage (SetFrac ident ((fromIntegral ey - leftmost)/widt)))
                                        (return ())
                                        
                   ml' <- if length wrs > 1 then Just `fmap` handle (dragUpDownPane ident delta split)
                                            else return Nothing
                   return (wrs, ml')
    message x | Just Shrink <- fromMessage x = Just (dragUpDownPane ident delta (split - delta))
              | Just Expand <- fromMessage x = Just (dragUpDownPane ident delta (split + delta))
              | Just (SetFrac ident' frac) <- fromMessage x, ident' == ident =
                                              Just (dragUpDownPane ident delta frac)
    message _ = Nothing

data SetFrac = SetFrac String Double deriving ( Show, Read, Eq, Typeable )
instance Message SetFrac
