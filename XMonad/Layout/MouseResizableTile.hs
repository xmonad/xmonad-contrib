{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MouseResizableTile
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
                                    MRTMessage (ShrinkSlave, ExpandSlave)
                                   ) where

import XMonad hiding (tile, splitVertically, splitHorizontallyBy)
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils

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
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- or
--
-- > myLayout = mouseResizableTileMirrored ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
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

data MRTMessage = SetMasterFraction Rational
                    | SetLeftSlaveFraction Int Rational
                    | SetRightSlaveFraction Int Rational
                    | ShrinkSlave
                    | ExpandSlave
                    deriving Typeable
instance Message MRTMessage

data DraggerInfo = MasterDragger Position Rational
                    | LeftSlaveDragger Position Rational Int
                    | RightSlaveDragger Position Rational Int
                    deriving (Show, Read)
type DraggerWithRect = (Rectangle, Glyph, DraggerInfo)
type DraggerWithWin = (Window, DraggerInfo)

data MouseResizableTile a = MRT { nmaster :: Int,
                                    masterFrac :: Rational,
                                    leftFracs :: [Rational],
                                    rightFracs :: [Rational],
                                    draggers :: [DraggerWithWin],
                                    focusPos :: Int,
                                    numWindows :: Int,
                                    isMirrored :: Bool
                                } deriving (Show, Read)

mrtFraction :: Rational
mrtFraction = 0.5
mrtDelta :: Rational
mrtDelta = 0.03
mrtDraggerOffset :: Position
mrtDraggerOffset = 3
mrtDraggerSize :: Dimension
mrtDraggerSize = 6
mrtHDoubleArrow :: Glyph
mrtHDoubleArrow = 108
mrtVDoubleArrow :: Glyph
mrtVDoubleArrow = 116

mouseResizableTile :: MouseResizableTile a
mouseResizableTile = MRT 1 mrtFraction [] [] [] 0 0 False

mouseResizableTileMirrored :: MouseResizableTile a
mouseResizableTileMirrored= MRT 1 mrtFraction [] [] [] 0 0 True

instance LayoutClass MouseResizableTile a where
    doLayout state sr (W.Stack w l r) =
        let wins = reverse l ++ w : r
            num = length wins
            sr' = mirrorAdjust sr (mirrorRect sr)
            (rects, preparedDraggers) = tile (nmaster state) (masterFrac state)
                                            (leftFracs state ++ repeat mrtFraction)
                                            (rightFracs state ++ repeat mrtFraction) sr' num
            rects' = map (mirrorAdjust id mirrorRect . sanitizeRectangle sr') rects
        in do
            mapM_ deleteDragger $ draggers state
            newDraggers <- mapM (createDragger sr . adjustForMirror (isMirrored state)) preparedDraggers
            return (zip wins rects', Just $ state { draggers = newDraggers,
                                                    focusPos = length l,
                                                    numWindows = length wins })
        where
            mirrorAdjust a b = if (isMirrored state)
                                then b
                                else a

    handleMessage state m
        | Just (IncMasterN d) <- fromMessage m =
            return $ Just $ state { nmaster = max 0 (nmaster state + d) }
        | Just Shrink <- fromMessage m =
            return $ Just $ state { masterFrac = max 0 (masterFrac state - mrtDelta) }
        | Just Expand <- fromMessage m =
            return $ Just $ state { masterFrac = min 1 (masterFrac state + mrtDelta) }
        | Just ShrinkSlave <- fromMessage m =
            return $ Just $ modifySlave state (-mrtDelta)
        | Just ExpandSlave <- fromMessage m =
            return $ Just $ modifySlave state mrtDelta
        | Just (SetMasterFraction f) <- fromMessage m =
            return $ Just $ state { masterFrac = max 0 (min 1 f) }
        | Just (SetLeftSlaveFraction pos f) <- fromMessage m =
            return $ Just $ state { leftFracs = replaceAtPos (leftFracs state) pos (max 0 (min 1 f)) }
        | Just (SetRightSlaveFraction pos f) <- fromMessage m =
            return $ Just $ state { rightFracs = replaceAtPos (rightFracs state) pos (max 0 (min 1 f)) }

        | Just e <- fromMessage m :: Maybe Event = handleResize (draggers state) (isMirrored state) e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ state { draggers = [] })
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ state { draggers = [] })
        where releaseResources = mapM_ deleteDragger $ draggers state
    handleMessage _ _ = return Nothing

    description _ = "MouseResizableTile"

adjustForMirror :: Bool -> DraggerWithRect -> DraggerWithRect
adjustForMirror False dragger = dragger
adjustForMirror True (draggerRect, draggerCursor, draggerInfo) =
        (mirrorRect draggerRect, draggerCursor', draggerInfo)
    where
        draggerCursor' = if (draggerCursor == mrtHDoubleArrow)
                            then mrtVDoubleArrow
                            else mrtHDoubleArrow

modifySlave :: MouseResizableTile a -> Rational-> MouseResizableTile a
modifySlave state delta =
    let pos = focusPos state
        num = numWindows state
        nmaster' = nmaster state
        leftFracs' = leftFracs state
        rightFracs' = rightFracs state
        draggersLeft = nmaster' - 1
        draggersRight = (num - nmaster') - 1
    in if pos < nmaster'
        then if draggersLeft > 0
                then let draggerPos = min (draggersLeft - 1) pos
                         oldFraction = (leftFracs' ++ repeat mrtFraction) !! draggerPos
                     in state { leftFracs = replaceAtPos leftFracs' draggerPos
                                            (max 0 (min 1 (oldFraction + delta))) }
                else state
        else if draggersRight > 0
                then let draggerPos = min (draggersRight - 1) (pos - nmaster')
                         oldFraction = (rightFracs' ++ repeat mrtFraction) !! draggerPos
                     in state { rightFracs = replaceAtPos rightFracs' draggerPos
                                            (max 0 (min 1 (oldFraction + delta))) }
                else state

replaceAtPos :: (Num t) => [Rational] -> t -> Rational -> [Rational]
replaceAtPos [] 0 x' = [x']
replaceAtPos [] pos x' = mrtFraction : replaceAtPos [] (pos - 1) x'
replaceAtPos (_:xs) 0 x' = x' : xs
replaceAtPos (x:xs) pos x' = x : replaceAtPos xs (pos -1 ) x'

sanitizeRectangle :: Rectangle -> Rectangle -> Rectangle
sanitizeRectangle (Rectangle sx sy swh sht) (Rectangle x y wh ht) =
    (Rectangle (within 0 (sx + fromIntegral swh) x) (within 0 (sy + fromIntegral sht) y)
                (within 1 swh wh) (within 1 sht ht))

within :: (Ord a) => a -> a -> a -> a
within low high a = max low $ min high a

tile :: Int -> Rational -> [Rational] -> [Rational] -> Rectangle -> Int -> ([Rectangle], [DraggerWithRect])
tile nmaster' masterFrac' leftFracs' rightFracs' sr num
    | num <= nmaster'       = splitVertically (take (num - 1) leftFracs') sr True 0
    | nmaster' == 0         = splitVertically (take (num - 1) rightFracs') sr False 0
    | otherwise             = (leftRects ++ rightRects, masterDragger : leftDraggers ++ rightDraggers)
    where ((sr1, sr2), masterDragger) = splitHorizontallyBy masterFrac' sr
          (leftRects, leftDraggers) = splitVertically (take (nmaster' - 1) leftFracs') sr1 True 0
          (rightRects, rightDraggers) = splitVertically (take (num - nmaster' - 1) rightFracs') sr2 False 0

splitVertically :: RealFrac r => [r] -> Rectangle -> Bool -> Int -> ([Rectangle], [DraggerWithRect])
splitVertically [] r _ _ = ([r], [])
splitVertically (f:fx) (Rectangle sx sy sw sh) isLeft num =
    let nextRect = Rectangle sx sy sw $ smallh - div mrtDraggerSize 2
        (otherRects, otherDragger) = splitVertically fx
                                        (Rectangle sx (sy + fromIntegral smallh + mrtDraggerOffset)
                                                    sw (sh - smallh - div mrtDraggerSize 2))
                                        isLeft (num + 1)
        draggerRect = Rectangle sx (sy + fromIntegral smallh - mrtDraggerOffset) sw mrtDraggerSize
        draggerInfo = if isLeft
                        then LeftSlaveDragger sy (fromIntegral sh) num
                        else RightSlaveDragger sy (fromIntegral sh) num
        nextDragger = (draggerRect, mrtVDoubleArrow, draggerInfo)
    in (nextRect : otherRects, nextDragger : otherDragger)
  where smallh = floor $ fromIntegral sh * f

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> ((Rectangle, Rectangle), DraggerWithRect)
splitHorizontallyBy f (Rectangle sx sy sw sh) = ((leftHalf, rightHalf), (draggerRect, mrtHDoubleArrow, draggerInfo))
  where leftw = floor $ fromIntegral sw * f
        leftHalf = Rectangle sx sy (leftw - mrtDraggerSize `div` 2) sh
        rightHalf = Rectangle (sx + fromIntegral leftw + mrtDraggerOffset) sy
                                (sw - fromIntegral leftw - mrtDraggerSize `div` 2) sh
        draggerRect = Rectangle (sx + fromIntegral leftw - mrtDraggerOffset) sy mrtDraggerSize sh
        draggerInfo = MasterDragger sx (fromIntegral sw)

createDragger :: Rectangle -> DraggerWithRect -> X DraggerWithWin
createDragger sr (draggerRect, draggerCursor, draggerInfo) = do
        draggerWin <- createInputWindow draggerCursor $ sanitizeRectangle sr draggerRect
        io . flip lowerWindow draggerWin =<< asks display
        return (draggerWin, draggerInfo)

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
