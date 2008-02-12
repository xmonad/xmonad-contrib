{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.MouseResize
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

import Control.Monad

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier

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
-- > myLayouts = mouseResize $ windowArrange $ layoutHook defaultConfig
--
-- and then:
--
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

mouseResize :: l a -> ModifiedLayout MouseResize l a
mouseResize = ModifiedLayout (MR [])

data MouseResize a = MR [((a,Rectangle),a)]
instance Show (MouseResize a) where show        _ = []
instance Read (MouseResize a) where readsPrec _ _ = []

instance LayoutModifier MouseResize Window where
    redoLayout (MR st) _ _ wrs
        | [] <- st  = do nst <- filterM (liftM not . isDecoration . fst) wrs >>= initState
                         return (wrs, Just $ MR nst)
        | otherwise = do nst <- filterM (liftM not . isDecoration . fst) wrs >>= processState
                         return (wrs, Just $ MR nst)
        where
          initState    ws = mapM createInputWindow ws
          processState ws = deleteWindows (map snd st) >> mapM createInputWindow ws

    handleMess (MR s) m
        | Just e <- fromMessage m :: Maybe Event = handleResize s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR [])
        where releaseResources = deleteWindows (map snd s)
    handleMess _ _ = return Nothing

handleResize :: [((Window,Rectangle),Window)] -> Event -> X ()
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
        getWin w (((win,r),w'):xs)
            | w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResize _ _ = return ()

createInputWindow :: (Window,Rectangle) -> X ((Window,Rectangle),Window)
createInputWindow (w,r@(Rectangle x y wh ht)) = do
  d  <- asks display
  let rect = Rectangle (x + fi wh - 5) (y + fi ht - 5) 10 10
  tw <- mkInputWindow d rect
  io $ selectInput d tw (exposureMask .|. buttonPressMask)
  showWindow tw
  return ((w,r),tw)

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
