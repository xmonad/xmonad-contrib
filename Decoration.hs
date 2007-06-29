{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Decoration
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module to be used to easily define decorations.
--
-----------------------------------------------------------------------------

module XMonadContrib.Decoration (
                                 -- * Usage
                                 -- $usage 
                                 newDecoration 
                                ) where

import Data.Bits ( (.|.) )
import Control.Monad.Reader ( asks )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( Event(AnyEvent,ButtonEvent), ev_subwindow, ev_event_type, ev_window )

import XMonadContrib.LayoutHelpers ( ModLay, layoutModify, idModDo )

import XMonad
import Operations ( UnDoLayout(UnDoLayout) )

-- $usage
-- You can use this module for writing other extensions.
-- See, for instance, "XMonadContrib.Tabbed"

newDecoration :: Window -> Rectangle -> Int -> Pixel -> Pixel -> String
              -> (Display -> Window -> GC -> FontStruct -> X ())
              -> X () -> Layout a -> X (Layout a)
newDecoration decfor (Rectangle x y w h) th fg bg fn draw click l = do
    d <- asks display
    rt <- asks theRoot
    win <- io $ createSimpleWindow d rt x y w h (fromIntegral th) fg bg
    io $ selectInput d win $ exposureMask .|. buttonPressMask
    io $ mapWindow d win

    let hook :: SomeMessage -> X (Maybe (ModLay a))
        hook sm  | Just e <- fromMessage sm           = handle_event e >> return Nothing
                 | Just UnDoLayout == fromMessage sm  = io (destroyWindow d win) >> return (Just id)
                 | otherwise                          = return Nothing

        handle_event (ButtonEvent {ev_subwindow = thisw,ev_event_type = t})
            | t == buttonPress && thisw == win        = click
        handle_event (ButtonEvent {ev_window = thisw,ev_event_type = t})
            | t == buttonPress && thisw == win        = click
        handle_event (AnyEvent {ev_window = thisw, ev_event_type = t})
            | thisw == win    && t == expose          = withGC win fn draw
            | thisw == decfor && t == propertyNotify  = withGC win fn draw
        handle_event _ = return ()

    return $ layoutModify idModDo hook l

-- FIXME: withGC should use bracket (but can't, unless draw is an IO thing)
withGC :: Drawable -> String -> (Display -> Drawable -> GC -> FontStruct -> X ()) -> X ()
withGC w fn f = withDisplay $ \d -> do gc <- io $ createGC d w
                                       let fontname = if fn == "" 
                                                      then "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                                                      else fn
                                       font <- io $ loadQueryFont d fontname
                                       io $ setFont d gc (fontFromFontStruct font)
                                       f d w gc font
                                       io $ freeGC d gc
                                       io $ freeFont d font
