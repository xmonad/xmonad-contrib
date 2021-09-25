{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DraggingVisualizer
-- Description :  Visualize the process of dragging a window.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A helper module to visualize the process of dragging a window by
-- making it follow the mouse cursor. See "XMonad.Layout.WindowSwitcherDecoration"
-- for a module that makes use of this.
--
-----------------------------------------------------------------------------

module XMonad.Layout.DraggingVisualizer
    ( draggingVisualizer,
      DraggingVisualizerMsg (..),
      DraggingVisualizer,
    ) where

import XMonad
import XMonad.Layout.LayoutModifier

newtype DraggingVisualizer a = DraggingVisualizer (Maybe (Window, Rectangle)) deriving ( Read, Show )
draggingVisualizer :: LayoutClass l Window => l Window -> ModifiedLayout DraggingVisualizer l Window
draggingVisualizer = ModifiedLayout $ DraggingVisualizer Nothing

data DraggingVisualizerMsg = DraggingWindow Window Rectangle
                                | DraggingStopped
                                deriving Eq
instance Message DraggingVisualizerMsg

instance LayoutModifier DraggingVisualizer Window where
    modifierDescription (DraggingVisualizer _) = "DraggingVisualizer"
    pureModifier (DraggingVisualizer (Just dragged@(draggedWin, _))) _ _ wrs =
            if draggedWin `elem` map fst wrs
                then (dragged : rest, Nothing)
                else (wrs, Just $ DraggingVisualizer Nothing)
        where
            rest = filter (\(w, _) -> w /= draggedWin) wrs
    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (DraggingVisualizer _) m = case fromMessage m of
        Just (DraggingWindow w rect) -> Just $ DraggingVisualizer $ Just (w, rect)
        Just DraggingStopped -> Just $ DraggingVisualizer Nothing
        _ -> Nothing
