{-# LANGUAGE MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.MouseResize
-- Description :  A layout modifier to resize windows with the mouse.
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier to resize windows with the mouse by grabbing the
-- window's lower right corner.
--
-- This module must be used together with "XMonad.Layout.WindowArranger".
-----------------------------------------------------------------------------

module XMonad.Actions.MouseResize
    ( -- * Usage:
      -- $usage
      mouseResize
    , MouseResize (..)
    ) where

import XMonad
import XMonad.Layout.Decoration

import XMonad.Layout.WindowArranger
import XMonad.Util.XUtils

-- $usage
-- Usually this module is used to create layouts, but you can also use
-- it to resize windows in any layout, together with the
-- "XMonad.Layout.WindowArranger". For usage example see
-- "XMonad.Layout.SimpleFloat" or "XMonad.Layout.DecorationMadness".
--
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.MouseResize
-- > import XMonad.Layout.WindowArranger
--
-- Then edit your @layoutHook@ by modifying a given layout:
--
-- > myLayout = mouseResize $ windowArrange $ layoutHook def
--
-- and then:
--
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

mouseResize :: l a -> ModifiedLayout MouseResize l a
mouseResize = ModifiedLayout (MR [])

newtype MouseResize a = MR [((a,Rectangle),Maybe a)]
instance Show (MouseResize a) where show        _ = ""
instance Read (MouseResize a) where readsPrec _ s = [(MR [], s)]

instance LayoutModifier MouseResize Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindow wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindow wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - 5) (y + fi ht - 5) 10 10

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR s) m
        | Just e <- fromMessage m :: Maybe Event = handleResize s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

handleResize :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResize st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy _ _) <- getWin ew st = do
                                        focus w
                                        mouseDrag (\x y -> do
                                                     let rect = Rectangle wx wy
                                                                (max 1 . fi $ x - wx)
                                                                (max 1 . fi $ y - wy)
                                                     sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResize _ _ = return ()

createInputWindow :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindow ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_bottom_right_corner
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

deleteInputWin :: Maybe Window -> X ()
deleteInputWin = maybe (return ()) deleteWindow

mkInputWindow :: Display -> Rectangle -> X Window
mkInputWindow d (Rectangle x y w h) = do
  rw <- asks theRoot
  let screen   = defaultScreenOfDisplay d
      visual   = defaultVisualOfScreen screen
      attrmask = cWOverrideRedirect
  io $ allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 0 inputOnly visual attrmask attributes
