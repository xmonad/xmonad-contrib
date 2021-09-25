{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MouseResizableTile
-- Description :  Like "XMonad.Layout.ResizableTile", but use the mouse to adjust the layout.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A layout in the spirit of "XMonad.Layout.ResizableTile", but with the option
-- to use the mouse to adjust the layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.MouseResizableTile (
                                    -- * Usage
                                    -- $usage
                                    mouseResizableTile,
                                    mouseResizableTileMirrored,
                                    MRTMessage (ShrinkSlave, ExpandSlave),

                                    -- * Parameters
                                    -- $mrtParameters
                                    nmaster,
                                    masterFrac,
                                    slaveFrac,
                                    fracIncrement,
                                    isMirrored,
                                    draggerType,
                                    DraggerType (..),
                                    MouseResizableTile,
                                   ) where

import XMonad hiding (tile, splitVertically, splitHorizontallyBy)
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils
import Graphics.X11 as X

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MouseResizableTile
--
-- Then edit your @layoutHook@ by adding the MouseResizableTile layout.
-- Either in its normal form or the mirrored version. (The mirror layout modifier
-- will not work correctly here because of the use of the mouse.)
--
-- > myLayout = mouseResizableTile ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You may also want to add the following key bindings:
--
-- > , ((modm,               xK_u), sendMessage ShrinkSlave) -- %! Shrink a slave area
-- > , ((modm,               xK_i), sendMessage ExpandSlave) -- %! Expand a slave area
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- $mrtParameters
-- The following functions are also labels for updating the @data@ (whose
-- representation is otherwise hidden) produced by 'mouseResizableTile'.
--
-- Usage:
--
-- > myLayout = mouseResizableTile{ masterFrac = 0.7,
-- >                                fracIncrement = 0.05,
-- >                                draggerType = BordersDragger }
-- >                |||  etc..

data MRTMessage = SetMasterFraction Rational
                    | SetLeftSlaveFraction Int Rational
                    | SetRightSlaveFraction Int Rational
                    | ShrinkSlave
                    | ExpandSlave
instance Message MRTMessage

data DraggerInfo = MasterDragger Position Rational
                    | LeftSlaveDragger Position Rational Int
                    | RightSlaveDragger Position Rational Int
                    deriving (Show, Read)
type DraggerWithRect = (Rectangle, Glyph, DraggerInfo)
type DraggerWithWin = (Window, DraggerInfo)

-- | Specifies the size of the clickable area between windows.
data DraggerType = FixedDragger
                    { gapWidth :: Dimension -- ^ width of a gap between windows
                    , draggerWidth :: Dimension -- ^ width of the dragger itself
                                                -- (will overlap windows if greater than gap)
                    }
                    | BordersDragger -- ^ no gaps, draggers overlap window borders
                    deriving (Show, Read)
type DraggerGeometry = (Position, Dimension, Position, Dimension)

data MouseResizableTile a = MRT { nmaster :: Int,
                                    -- ^ Get/set the number of windows in
                                    -- master pane (default: 1).
                                    masterFrac :: Rational,
                                    -- ^ Get/set the proportion of screen
                                    -- occupied by master pane (default: 1/2).
                                    slaveFrac :: Rational,
                                    -- ^ Get/set the proportion of remaining
                                    -- space in a column occupied by a slave
                                    -- window (default: 1/2).
                                    fracIncrement :: Rational,
                                    -- ^ Get/set the increment used when
                                    -- modifying masterFrac/slaveFrac by the
                                    -- Shrink, Expand, etc. messages (default:
                                    -- 3/100).
                                    leftFracs :: [Rational],
                                    rightFracs :: [Rational],
                                    draggers :: [DraggerWithWin],
                                    draggerType :: DraggerType,
                                    -- ^ Get/set dragger and gap dimensions
                                    -- (default: FixedDragger 6 6).
                                    focusPos :: Int,
                                    numWindows :: Int,
                                    isMirrored :: Bool
                                    -- ^ Get/set whether the layout is
                                    -- mirrored (default: False).
                                } deriving (Show, Read)

mouseResizableTile :: MouseResizableTile a
mouseResizableTile = MRT 1 0.5 0.5 0.03 [] [] [] (FixedDragger 6 6) 0 0 False

-- | May be removed in favor of @mouseResizableTile { isMirrored = True }@
mouseResizableTileMirrored :: MouseResizableTile a
mouseResizableTileMirrored = mouseResizableTile { isMirrored = True }

instance LayoutClass MouseResizableTile Window where
    doLayout st sr (W.Stack w l r) = do
        drg <- draggerGeometry $ draggerType st
        let wins = reverse l ++ w : r
            num = length wins
            sr' = mirrorAdjust sr (mirrorRect sr)
            (rects, preparedDraggers) = tile (nmaster st) (masterFrac st)
                                            (leftFracs st ++ repeat (slaveFrac st))
                                            (rightFracs st ++ repeat (slaveFrac st)) sr' num drg
            rects' = map (mirrorAdjust id mirrorRect . sanitizeRectangle sr') rects
        mapM_ deleteDragger $ draggers st
        (draggerWrs, newDraggers) <- unzip <$> mapM
                                        (createDragger sr . adjustForMirror (isMirrored st))
                                        preparedDraggers
        return (draggerWrs ++ zip wins rects', Just $ st { draggers = newDraggers,
                                                              focusPos = length l,
                                                              numWindows = length wins })
        where
            mirrorAdjust a b = if isMirrored st
                                then b
                                else a

    handleMessage st m
        | Just (IncMasterN d) <- fromMessage m =
            return $ Just $ st { nmaster = max 0 (nmaster st + d) }
        | Just Shrink <- fromMessage m =
            return $ Just $ st { masterFrac = max 0 (masterFrac st - fracIncrement st) }
        | Just Expand <- fromMessage m =
            return $ Just $ st { masterFrac = min 1 (masterFrac st + fracIncrement st) }
        | Just ShrinkSlave <- fromMessage m =
            return $ Just $ modifySlave st (- fracIncrement st)
        | Just ExpandSlave <- fromMessage m =
            return $ Just $ modifySlave st (fracIncrement st)
        | Just (SetMasterFraction f) <- fromMessage m =
            return $ Just $ st { masterFrac = max 0 (min 1 f) }
        | Just (SetLeftSlaveFraction pos f) <- fromMessage m =
            return $ Just $ st { leftFracs = replaceAtPos (slaveFrac st)
                (leftFracs st) pos (max 0 (min 1 f)) }
        | Just (SetRightSlaveFraction pos f) <- fromMessage m =
            return $ Just $ st { rightFracs = replaceAtPos (slaveFrac st)
                (rightFracs st) pos (max 0 (min 1 f)) }

        | Just e <- fromMessage m :: Maybe Event = handleResize (draggers st) (isMirrored st) e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ st { draggers = [] })
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ st { draggers = [] })
        where releaseResources = mapM_ deleteDragger $ draggers st
    handleMessage _ _ = return Nothing

    description st = mirror "MouseResizableTile"
        where mirror = if isMirrored st then ("Mirror " ++) else id

draggerGeometry :: DraggerType -> X DraggerGeometry
draggerGeometry (FixedDragger g d) =
    return (fromIntegral $ g `div` 2, g, fromIntegral $ d `div` 2, d)
draggerGeometry BordersDragger = do
    wins <- gets windowset
    w <- case W.peek wins of
          Just win -> getBorderWidth win
          _        -> asks (borderWidth . config)
    return (0, 0, fromIntegral w, 2*w)

getBorderWidth :: Window -> X Dimension
getBorderWidth win = do
    d <- asks display
    (_,_,_,_,_,w,_) <- io $ X.getGeometry d win
    return w

adjustForMirror :: Bool -> DraggerWithRect -> DraggerWithRect
adjustForMirror False dragger = dragger
adjustForMirror True (draggerRect, draggerCursor, draggerInfo) =
        (mirrorRect draggerRect, draggerCursor', draggerInfo)
    where
        draggerCursor' = if draggerCursor == xC_sb_h_double_arrow
                            then xC_sb_v_double_arrow
                            else xC_sb_h_double_arrow

modifySlave :: MouseResizableTile a -> Rational -> MouseResizableTile a
modifySlave st delta =
    let pos = focusPos st
        num = numWindows st
        nmaster' = nmaster st
        leftFracs' = leftFracs st
        rightFracs' = rightFracs st
        slFrac = slaveFrac st
        draggersLeft = nmaster' - 1
        draggersRight = (num - nmaster') - 1
    in if pos < nmaster'
        then if draggersLeft > 0
                then let draggerPos = min (draggersLeft - 1) pos
                         oldFraction = (leftFracs' ++ repeat slFrac) !! draggerPos
                     in st { leftFracs = replaceAtPos slFrac leftFracs' draggerPos
                                            (max 0 (min 1 (oldFraction + delta))) }
                else st
        else if draggersRight > 0
                then let draggerPos = min (draggersRight - 1) (pos - nmaster')
                         oldFraction = (rightFracs' ++ repeat slFrac) !! draggerPos
                     in st { rightFracs = replaceAtPos slFrac rightFracs' draggerPos
                                            (max 0 (min 1 (oldFraction + delta))) }
                else st

replaceAtPos :: (Num t, Eq t) => Rational -> [Rational] -> t -> Rational -> [Rational]
replaceAtPos _ [] 0 x' = [x']
replaceAtPos d [] pos x' = d : replaceAtPos d [] (pos - 1) x'
replaceAtPos _ (_:xs) 0 x' = x' : xs
replaceAtPos d (x:xs) pos x' = x : replaceAtPos d xs (pos -1 ) x'

sanitizeRectangle :: Rectangle -> Rectangle -> Rectangle
sanitizeRectangle (Rectangle sx sy swh sht) (Rectangle x y wh ht) =
    Rectangle (within 0 (sx + fromIntegral swh) x) (within 0 (sy + fromIntegral sht) y)
                (within 1 swh wh) (within 1 sht ht)

within :: (Ord a) => a -> a -> a -> a
within low high a = max low $ min high a

tile :: Int -> Rational -> [Rational] -> [Rational] -> Rectangle -> Int -> DraggerGeometry -> ([Rectangle], [DraggerWithRect])
tile nmaster' masterFrac' leftFracs' rightFracs' sr num drg
    | num <= nmaster'       = splitVertically (take (num - 1) leftFracs') sr True 0 drg
    | nmaster' == 0         = splitVertically (take (num - 1) rightFracs') sr False 0 drg
    | otherwise             = (leftRects ++ rightRects, masterDragger : leftDraggers ++ rightDraggers)
    where ((sr1, sr2), masterDragger) = splitHorizontallyBy masterFrac' sr drg
          (leftRects, leftDraggers) = splitVertically (take (nmaster' - 1) leftFracs') sr1 True 0 drg
          (rightRects, rightDraggers) = splitVertically (take (num - nmaster' - 1) rightFracs') sr2 False 0 drg

splitVertically :: RealFrac r => [r] -> Rectangle -> Bool -> Int -> DraggerGeometry -> ([Rectangle], [DraggerWithRect])
splitVertically [] r _ _ _ = ([r], [])
splitVertically (f:fx) (Rectangle sx sy sw sh) isLeft num drg@(drOff, drSz, drOff2, drSz2) =
    let nextRect = Rectangle sx sy sw $ smallh - div drSz 2
        (otherRects, otherDragger) = splitVertically fx
                                        (Rectangle sx (sy + fromIntegral smallh + drOff)
                                                    sw (sh - smallh - div drSz 2))
                                        isLeft (num + 1) drg
        draggerRect = Rectangle sx (sy + fromIntegral smallh - drOff2) sw drSz2
        draggerInfo = if isLeft
                        then LeftSlaveDragger sy (fromIntegral sh) num
                        else RightSlaveDragger sy (fromIntegral sh) num
        nextDragger = (draggerRect, xC_sb_v_double_arrow, draggerInfo)
    in (nextRect : otherRects, nextDragger : otherDragger)
  where smallh = floor $ fromIntegral sh * f

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> DraggerGeometry -> ((Rectangle, Rectangle), DraggerWithRect)
splitHorizontallyBy f (Rectangle sx sy sw sh) (drOff, drSz, drOff2, drSz2) =
    ((leftHalf, rightHalf), (draggerRect, xC_sb_h_double_arrow, draggerInfo))
  where leftw = floor $ fromIntegral sw * f
        leftHalf = Rectangle sx sy (leftw - drSz `div` 2) sh
        rightHalf = Rectangle (sx + fromIntegral leftw + drOff) sy
                                (sw - fromIntegral leftw - drSz `div` 2) sh
        draggerRect = Rectangle (sx + fromIntegral leftw - drOff2) sy drSz2 sh
        draggerInfo = MasterDragger sx (fromIntegral sw)

createDragger :: Rectangle -> DraggerWithRect -> X ((Window, Rectangle), DraggerWithWin)
createDragger sr (draggerRect, draggerCursor, draggerInfo) = do
        let draggerRect' = sanitizeRectangle sr draggerRect
        draggerWin <- createInputWindow draggerCursor draggerRect'
        return ((draggerWin, draggerRect'), (draggerWin, draggerInfo))

deleteDragger :: DraggerWithWin -> X ()
deleteDragger (draggerWin, _) = deleteWindow draggerWin

handleResize :: [DraggerWithWin] -> Bool -> Event -> X ()
handleResize draggers' isM ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress, Just x <- lookup ew draggers' = case x of
        MasterDragger     lb r     -> mouseDrag' id   lb r  SetMasterFraction
        LeftSlaveDragger  lb r num -> mouseDrag' flip lb r (SetLeftSlaveFraction num)
        RightSlaveDragger lb r num -> mouseDrag' flip lb r (SetRightSlaveFraction num)
    where
        chooseAxis isM' axis1 axis2 = if isM' then axis2 else axis1
        mouseDrag' flp lowerBound range msg = flip mouseDrag (return ()) $ \x y -> do
                let axis = flp (chooseAxis isM) x y
                    fraction = fromIntegral (axis - lowerBound) / range
                sendMessage (msg fraction)

handleResize _ _ _ = return ()

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
