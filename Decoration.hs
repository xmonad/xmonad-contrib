{-# OPTIONS -fglasgow-exts #-}
module XMonadContrib.Decoration ( newDecoration ) where

import Data.Bits ( (.|.) )
import Control.Monad.Reader ( asks )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( Event(AnyEvent,ButtonEvent), ev_subwindow, ev_event_type, ev_window )

import XMonadContrib.LayoutHooks

import XMonad
import Operations ( UnDoLayout(UnDoLayout) )

newDecoration :: Window -> Rectangle -> Int -> Pixel -> Pixel
              -> (Display -> Window -> GC -> X ()) -> X () -> X Window
newDecoration decfor (Rectangle x y w h) th fg bg draw click = do
    d <- asks display
    rt <- asks theRoot
    win <- io $ createSimpleWindow d rt x y w h (fromIntegral th) fg bg
    io $ selectInput d win $ exposureMask .|. buttonPressMask
    io $ mapWindow d win

    let hook                                          :: SomeMessage -> X Bool
        hook sm  | Just e <- fromMessage sm           = handle_event e >> return True
                 | Just UnDoLayout == fromMessage sm  = io (destroyWindow d win) >> return False
                 | otherwise                          = return True

        handle_event (ButtonEvent {ev_subwindow = thisw,ev_event_type = t})
            | t == buttonPress && thisw == win        = click
        handle_event (ButtonEvent {ev_window = thisw,ev_event_type = t})
            | t == buttonPress && thisw == win        = click
        handle_event (AnyEvent {ev_window = thisw, ev_event_type = t})
            | thisw == win    && t == expose          = withGC win draw
            | thisw == decfor && t == propertyNotify  = withGC win draw
        handle_event _ = return ()

    addLayoutMessageHook hook

    return win

-- FIXME: withGC should use bracket (but can't, unless draw is an IO thing)
withGC :: Drawable -> (Display -> Drawable -> GC -> X ()) -> X ()
withGC w f = withDisplay $ \d -> do gc <- io $ createGC d w
                                    f d w gc
                                    io $ freeGC d gc
