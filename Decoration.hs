module XMonadContrib.Decoration ( newDecoration ) where

import qualified Data.Map as M
import Control.Monad.Reader ( asks )
import Control.Monad.State ( modify, gets )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( Event(AnyEvent,ButtonEvent), ev_subwindow, ev_event_type, ev_window )

import XMonad
import Operations ( ModifyWindows(ModifyWindows) )
import qualified StackSet as W

newDecoration :: Rectangle -> Int -> Pixel -> Pixel
              -> (Display -> Window -> GC -> X ()) -> X () -> X Window
newDecoration (Rectangle x y w h) th fg bg draw click =
    do d <- asks display
       rt <- asks theRoot
       n <- (W.tag . W.workspace . W.current) `fmap` gets windowset
       Just (l,ls) <- M.lookup n `fmap` gets layouts
       win <- io $ createSimpleWindow d rt x y w h (fromIntegral th) fg bg
       io $ mapWindow d win
       let draw' = withGC win draw
           modml :: (SomeMessage -> X (Maybe Layout)) -> SomeMessage -> X (Maybe Layout)
           modml oldml m | Just ModifyWindows == fromMessage m = io (destroyWindow d win) >> oldml m
                         | Just e <- fromMessage m = handle_event e >> oldml m
                         | otherwise = fmap modl `fmap` oldml m
           modl :: Layout -> Layout
           modl oldl = oldl { modifyLayout = modml (modifyLayout oldl) }
           handle_event (ButtonEvent {ev_subwindow = thisw,ev_event_type = t})
               | t == buttonPress && thisw == win = click
           handle_event (ButtonEvent {ev_window = thisw,ev_event_type = t})
               | t == buttonPress && thisw == win = click
           handle_event (AnyEvent {ev_window = thisw}) | thisw == win = draw'
           handle_event _ = return ()
       draw'
       modify $ \s -> s { layouts = M.insert n (modl l,ls) (layouts s) }
       return win

-- FIXME: withGC should use bracket (but can't, unless draw is an IO thing)
withGC :: Drawable -> (Display -> Drawable -> GC -> X ()) -> X ()
withGC w f = withDisplay $ \d -> do gc <- io $ createGC d w
                                    f d w gc
                                    io $ freeGC d gc
