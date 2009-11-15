{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BorderResize
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- This layout modifier will allow to resize windows by dragging their
-- borders with the mouse. However, it only works in layouts or modified
-- layouts that react to the 'SetGeometry' message.
-- "XMonad.Layout.WindowArranger" can be used to create such a setup,
-- but it is probably must useful in a floating layout such as
-- "XMonad.Layout.PositionStoreFloat" with which it has been mainly tested.
-- See the documentation of PositionStoreFloat for a typical usage example.
--
-----------------------------------------------------------------------------

module XMonad.Layout.BorderResize
    ( -- * Usage
      -- $usage
      borderResize
    , BorderResize (..)
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.WindowArranger
import XMonad.Util.XUtils
import Control.Monad(when,forM)
import Control.Arrow(first)
import Control.Applicative((<$>))

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.BorderResize
-- > myLayout = borderResize (... layout setup that reacts to SetGeometry ...)
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--

data BorderInfo = RightSideBorder Window Rectangle
                    | LeftSideBorder Window Rectangle
                    | TopSideBorder Window Rectangle
                    | BottomSideBorder Window Rectangle
                    deriving (Show, Read, Eq)
type BorderWithRect = (Rectangle, Rectangle, Glyph, BorderInfo)
type BorderWithWin = (Window, BorderInfo)

data BorderResize a = BR [BorderWithWin] deriving (Show, Read)

brBorderOffset :: Position
brBorderOffset = 5
brBorderSize :: Dimension
brBorderSize = 10

brCursorRightSide :: Glyph
brCursorRightSide = 96
brCursorLeftSide :: Glyph
brCursorLeftSide = 70
brCursorTopSide :: Glyph
brCursorTopSide = 138
brCursorBottomSide :: Glyph
brCursorBottomSide = 16

borderResize :: l a -> ModifiedLayout BorderResize l a
borderResize = ModifiedLayout (BR [])

instance LayoutModifier BorderResize Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (BR borders) _ _ wrs = do
        let preparedBorders = for wrs $ \wr -> (wr, prepareBorders wr)
        mapM_ deleteBorder borders
        newBorders <- forM preparedBorders $ \(wr, (b1, b2, b3, b4)) ->
                        first (++[wr]) . unzip <$> mapM createBorder [b1,b2,b3,b4]
        let wrs' = concat $ map fst newBorders
            newBordersSerialized = concat $ map snd newBorders
        return (wrs', Just $ BR newBordersSerialized)
            -- What we return is the original wrs with the new border
            -- windows inserted at the correct positions - this way, the core
            -- will restack the borders correctly.
            -- We also return information about our borders, so that we
            -- can handle events that they receive and destroy them when
            -- they are no longer needed.

    handleMess (BR borders) m
        | Just e <- fromMessage m :: Maybe Event = handleResize borders e >> return Nothing
        | Just _ <- fromMessage m :: Maybe LayoutMessages =
            mapM_ deleteBorder borders >> return (Just $ BR [])
    handleMess _ _ = return Nothing

prepareBorders :: (Window, Rectangle) -> (BorderWithRect, BorderWithRect, BorderWithRect, BorderWithRect)
prepareBorders (w, r@(Rectangle x y wh ht)) =
    ((r, (Rectangle (x + fi wh - brBorderOffset) y brBorderSize ht) , brCursorRightSide     , RightSideBorder w r),
     (r, (Rectangle (x - brBorderOffset) y brBorderSize ht)         , brCursorLeftSide      , LeftSideBorder w r),
     (r, (Rectangle x (y - brBorderOffset) wh brBorderSize)         , brCursorTopSide       , TopSideBorder w r),
     (r, (Rectangle x (y + fi ht - brBorderOffset) wh brBorderSize) , brCursorBottomSide    , BottomSideBorder w r)
    )

handleResize :: [BorderWithWin] -> Event -> X ()
handleResize borders ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress, Just edge <- lookup ew borders =
    case edge of
        RightSideBorder hostWin (Rectangle hx hy _ hht) ->
            mouseDrag (\x _ -> do
                            let nwh = max 1 $ fi (x - hx)
                                rect = Rectangle hx hy nwh hht
                            focus hostWin
                            when (x - hx > 0) $ sendMessage (SetGeometry rect)) (focus hostWin)
        LeftSideBorder hostWin (Rectangle hx hy hwh hht) ->
            mouseDrag (\x _ -> do
                            let nx = max 0 $ min (hx + fi hwh) $ x
                                nwh = max 1 $ hwh + fi (hx - x)
                                rect = Rectangle nx hy nwh hht
                            focus hostWin
                            when (x < hx + fi hwh) $ sendMessage (SetGeometry rect)) (focus hostWin)
        TopSideBorder hostWin (Rectangle hx hy hwh hht) ->
            mouseDrag (\_ y -> do
                            let ny = max 0 $ min (hy + fi hht) $ y
                                nht = max 1 $ hht + fi (hy - y)
                                rect = Rectangle hx ny hwh nht
                            focus hostWin
                            when (y < hy + fi hht) $ sendMessage (SetGeometry rect)) (focus hostWin)
        BottomSideBorder hostWin (Rectangle hx hy hwh _) ->
            mouseDrag (\_ y -> do
                            let nht = max 1 $ fi (y - hy)
                                rect = Rectangle hx hy hwh nht
                            focus hostWin
                            when (y - hy > 0) $ sendMessage (SetGeometry rect)) (focus hostWin)
handleResize _ _ = return ()

createBorder :: BorderWithRect -> X (((Window, Rectangle), BorderWithWin))
createBorder (_, borderRect, borderCursor, borderInfo) = do
    borderWin <- createInputWindow borderCursor borderRect
    return ((borderWin, borderRect), (borderWin, borderInfo))

deleteBorder :: BorderWithWin -> X ()
deleteBorder (borderWin, _) = deleteWindow borderWin

createInputWindow :: Glyph -> Rectangle -> X Window
createInputWindow cursorGlyph r = withDisplay $ \d -> do
    win <- mkInputWindow d r
    io $ selectInput d win (exposureMask .|. buttonPressMask)
    cursor <- io $ createFontCursor d cursorGlyph
    io $ defineCursor d win cursor
    io $ freeCursor d cursor
    showWindow win
    return win

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

for :: [a] -> (a -> b) -> [b]
for = flip map
