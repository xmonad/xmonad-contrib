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
--
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
import Operations ( Resize(..), splitHorizontallyBy, initColor, mouseDrag, sendMessage, mirrorRect )
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
halfHandleWidth = 1

handleColor :: String
handleColor = "#000000"

dragPane :: String -> Double -> Double -> Layout a
dragPane = dragPane' id

dragUpDownPane :: String -> Double -> Double -> Layout a
dragUpDownPane = dragPane' mirrorRect

dragPane' :: (Rectangle -> Rectangle) -> String -> Double -> Double -> Layout a
dragPane' mirror ident delta split = Layout { doLayout = dolay, modifyLayout = return . message }
 where
    dolay r s = do handlec <- withDisplay $ \dpy -> io $ initColor dpy handleColor
                   root <- asks theRoot
                   let r' = mirror r
		       (left', right') = splitHorizontallyBy split r'
                       leftmost = fromIntegral $ case r' of Rectangle x _ _ _ -> x
                       widt = fromIntegral $ case r' of Rectangle _ _ w _ -> w
                       left = case left' of Rectangle x y w h ->
		                              mirror $ Rectangle x y (w-halfHandleWidth) h
                       right = case right' of
                               Rectangle x y w h ->
			         mirror $ Rectangle (x+halfHandleWidth) y (w-halfHandleWidth) h
                       handr = case left' of
                               Rectangle x y w h ->
                                 mirror $ Rectangle (x + fromIntegral w - halfHandleWidth) y (2*halfHandleWidth) h
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
                                        
                   ml' <- if length wrs > 1 then Just `fmap` handle (dragPane' mirror ident delta split)
                                            else return Nothing
                   return (wrs, ml')
    message x | Just Shrink <- fromMessage x = Just (dragPane' mirror ident delta (split - delta))
              | Just Expand <- fromMessage x = Just (dragPane' mirror ident delta (split + delta))
              | Just (SetFrac ident' frac) <- fromMessage x, ident' == ident =
                                              Just (dragPane' mirror ident delta frac)
    message _ = Nothing

data SetFrac = SetFrac String Double deriving ( Show, Read, Eq, Typeable )
instance Message SetFrac
