{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Float.SimplestFloatDec
-- Copyright   :  (c) 2015 Jeffrey Lyman
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  JLyman@macalester.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- A Floating window decorater that draws inspiration from 
-- Andrea Rossato/Jan Vornberger's Layout.Decoration module
-----------------------------------------------------------------------------

module XMonad.Float.SimplestFloatDec 
    ( SimplestDec(..)
    , simplestDec
    , Theme(..)
    , def 
    ) where

import XMonad
import XMonad.Layout.Decoration hiding (shrink)
import Control.Monad
import Control.Applicative ((<$>))
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Util.Invisible
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Bits
import Data.List (foldl')
import Graphics.X11.Xlib.Window

-- $usage
-- This module provides a simple decoration that allows users to move windows by
-- dragging the decoration and delete windows clicking the window decoration with
-- the second mouse button

-- | a decoration consists of a list of 'Query's to ignore, a list of 'Theme' 
-- a 'Shrinker' and a 'XMonadFont' used to draw the decorations
--
-- For more information on 'Shrinker's and 'Themes' see "XMonad.Layout.Decoration"
data SimplestDec a = SDec [Query Bool] Theme DefaultShrinker (Invisible Maybe XMonadFont) 

-- | given a list of 'Query's to ignore and a 'Theme' create a FloatDec
simplestDec :: [Query Bool] -> Theme -> FloatDec Window
simplestDec is t = FloatDec (SDec is t shrinkText (I Nothing))

-- | whether the given window should be decorated
shouldDec :: [Query Bool] -> Window -> X Bool
shouldDec qs w = not <$> foldl' p (return False) qs
    where p tv q = liftM2 (||) tv (runQuery q w)

instance FloatClass SimplestDec Window where
    handleFloatMessage (SDec _ (Theme {decoHeight = h}) _ _) m 
        | Just e@(ButtonEvent { ev_event_type = buttonPress, ev_button = b,
                                ev_window = w}) <- fromMessage m 
            = do when (b == 1) $ decMovesWindow (0, fromIntegral h) w 
                 when (b == 2) $ decDeletesWindow w
                 return Nothing
    handleFloatMessage _ _ = return Nothing
   
    createFDec os@(SDec i t s _) ow = do
        dec <- shouldDec i ow
        if dec
            then do
                (f, ns) <- initFont os
                dr <- decSize t ow
                dw <- createDecoWindow t dr
                showWindow dw 
                updateFDec t s f ow dw dr
                return (Just dw, ns)
            else return (Nothing, Nothing)

    moveFDec os@(SDec _ t s _) ow dw = do
        (f, ns) <- initFont os
        dr@(Rectangle x y w h) <- decSize t ow
        withDisplay $ \d -> do
            io $ raiseWindow d dw 
            io $ moveResizeWindow d dw x y w h
        updateFDec t s f ow dw dr
        return ns

    finishDecDrag _ ow _ = do
        windows $ W.shiftMaster . W.focusWindow ow
        return Nothing

    startDecDrag _ _ dw = do
        withDisplay $ \d -> io $ raiseWindow d dw
        return Nothing

    whileDecDrag (SDec _ (Theme {decoHeight=h}) _ _) _ dw = 
        return $ followWindow h dw

--------------------------------------------------------------
--                     Actions                              -- 
--------------------------------------------------------------
  
-- | Given a decoration's height, move it so that it matches it's
-- parent's position
followWindow :: Dimension -> Window -> Rectangle -> X ()
followWindow h dw (Rectangle x y w _) = withDisplay $ \d ->
    io $ moveResizeWindow d dw x (y - fromIntegral h) w h

-- | given an theme and the parent window, return a rectangle
-- representing the dimensions of it's decoration
decSize :: Theme -> Window -> X Rectangle
decSize Theme {decoHeight = h} ow = do
        ws <- gets windowset
        let sr = screenRect $ W.screenDetail $ W.current ws
            Just rr = M.lookup ow (W.floating ws)
            or@(Rectangle x y w _) = scaleRationalRect sr rr
        return $ Rectangle x (y - fromIntegral h) w (fromIntegral h)

-- | given a theme an a bounding rectangle, create a window for the decoration
createDecoWindow :: Theme -> Rectangle -> X Window
createDecoWindow t r = let mask = Just (exposureMask .|. buttonPressMask) in
                       createNewWindow r mask (inactiveColor t) True

-- | given the amount the parent should be offset from the decoration, move the
-- parrent window so that it follows the when the float window is dragged
decMovesWindow :: (Position, Position) -> Window -> X ()
decMovesWindow (dx,dy) dw = whenDec dw $ \ow -> withDisplay $ \d -> do
    io $ raiseWindow d dw
    io $ raiseWindow d ow
    (WindowAttributes wax way _ _ _ _ _) <- io $ getWindowAttributes d dw
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d dw
    let ox = fromIntegral ox'
        oy = fromIntegral oy'
        nx ex = fromIntegral wax + fromIntegral (ex - ox)
        ny ey = fromIntegral way + fromIntegral (ey - oy)
    mouseDrag (\ex ey -> do io $ moveWindow d dw (nx ex) (ny ey)
                            io $ moveWindow d ow (dx + nx ex) (dy + ny ey) )
              (float ow >> windows (W.shiftMaster . W.focusWindow ow))

-- | given a window, if it is a decoration, delete it's parent
decDeletesWindow :: Window -> X ()
decDeletesWindow w = whenDec w $ \ow -> killWindow ow

--------------------------------------------------------------
--                     Drawing                              -- 
--------------------------------------------------------------
 
 -- | ensure that the font we use for the decorations is loaded
initFont :: SimplestDec Window -> X (XMonadFont, Maybe (SimplestDec Window))
initFont (SDec _ _ _ (I (Just xf))) = return (xf, Nothing)
initFont (SDec i t s (I Nothing))  = do
    xf <- initXMF (fontName t)
    return (xf, Just (SDec i t s (I (Just xf))))

-- | Paint the decoration to match the theme
updateFDec :: Shrinker s => Theme -> s -> XMonadFont -> Window -> Window -> Rectangle -> X ()
updateFDec t sh fs ow dw (Rectangle _ _ wh ht) = do
  nw  <- getName ow
  ur  <- readUrgents
  dpy <- asks display
  let focusColor win ic ac uc = (maybe ic (\focusw -> case () of
                                                       _ | focusw == win -> ac
                                                         | win `elem` ur -> uc
                                                         | otherwise     -> ic) . W.peek)
                                `fmap` gets windowset
  (bc,borderc,tc) <- focusColor ow (inactiveColor t, inactiveBorderColor t, inactiveTextColor t)
                                  (activeColor   t, activeBorderColor   t, activeTextColor   t)
                                  (urgentColor   t, urgentBorderColor   t, urgentTextColor   t)
  let s = shrinkIt sh
  name <- shrinkWhile s (\n -> do size <- io $ textWidthXMF dpy fs n
                                  return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) (show nw)
  let als = AlignCenter : map snd (windowTitleAddons t)
      strs = name : map fst (windowTitleAddons t)
      i_als = map snd (windowTitleIcons t)
      icons = map fst (windowTitleIcons t)
  paintTextAndIcons dw fs wh ht 1 bc borderc tc bc als strs i_als icons
