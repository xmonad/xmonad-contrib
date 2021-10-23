{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp, PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutHints
-- Description :  Make layouts respect size hints.
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : unportable
--
-- Make layouts respect size hints.
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutHints
    ( -- * usage
      -- $usage
      layoutHints
    , layoutHintsWithPlacement
    , layoutHintsToCenter
    , LayoutHints
    , LayoutHintsToCenter
    , hintsEventHook
    -- * For developers
    , placeRectangle
    )  where

import XMonad(LayoutClass(runLayout), mkAdjust, Window,
              Dimension, Position, Rectangle(Rectangle), D,
              X, refresh, Event(..), propertyNotify, wM_NORMAL_HINTS,
              (<&&>), io, applySizeHints, whenX, isClient, withDisplay,
              getWMNormalHints, WindowAttributes(..))
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Layout.Decoration(isInStack)
import XMonad.Layout.LayoutModifier(ModifiedLayout(..),
                                    LayoutModifier(modifyLayout, redoLayout, modifierDescription))
import XMonad.Util.Types(Direction2D(..))
import Control.Arrow(Arrow((***), first, second))

import Data.Set (Set)
import qualified Data.Set as Set

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutHints
--
-- Then edit your @layoutHook@ by adding the 'layoutHints' layout modifier
-- to some layout:
--
-- > myLayout = layoutHints (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- Or, to center the adapted window in its available area:
--
-- > myLayout = layoutHintsWithPlacement (0.5, 0.5) (Tall 1 (3/100) (1/2))
-- >                   ||| Full ||| etc..
--
-- Or, to make a reasonable attempt to eliminate gaps between windows:
--
-- > myLayout = layoutHintsToCenter (Tall 1 (3/100) (1/2))
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- To make XMonad reflect changes in window hints immediately, add
-- 'hintsEventHook' to your 'handleEventHook'.
--
-- > myHandleEventHook = hintsEventHook <+> ...
-- >
-- > main = xmonad def { handleEventHook = myHandleEventHook
-- >                   , ... }

layoutHints :: (LayoutClass l a) => l a -> ModifiedLayout LayoutHints l a
layoutHints = ModifiedLayout (LayoutHints (0, 0))

-- | @layoutHintsWithPlacement (rx, ry) layout@ will adapt the sizes of a layout's
-- windows according to their size hints, and position them inside their
-- originally assigned area according to the @rx@ and @ry@ parameters.
-- (0, 0) places the window at the top left, (1, 0) at the top right, (0.5, 0.5)
-- at the center, etc.
layoutHintsWithPlacement :: (LayoutClass l a) => (Double, Double)
                         -> l a -> ModifiedLayout LayoutHints l a
layoutHintsWithPlacement rs = ModifiedLayout (LayoutHints rs)

-- | @layoutHintsToCenter layout@ applies hints, sliding the window to the
-- center of the screen and expanding its neighbors to fill the gaps. Windows
-- are never expanded in a way that increases overlap.
--
-- @layoutHintsToCenter@ only makes one pass at resizing the neighbors of
-- hinted windows, so with some layouts (ex. the arrangement with two 'Mirror'
-- 'Tall' stacked vertically), @layoutHintsToCenter@ may leave some gaps.
-- Simple layouts like 'Tall' are unaffected.
layoutHintsToCenter :: (LayoutClass l a) => l a -> ModifiedLayout LayoutHintsToCenter l a
layoutHintsToCenter = ModifiedLayout LayoutHintsToCenter

newtype LayoutHints a = LayoutHints (Double, Double)
                     deriving (Read, Show)

instance LayoutModifier LayoutHints Window where
    modifierDescription _ = "Hinted"
    redoLayout _ _ Nothing  xs = return (xs, Nothing)
    redoLayout (LayoutHints al) _ (Just s) xs
        = do xs' <- mapM (\x@(_, r) -> second (placeRectangle al r) <$> applyHint x) xs
             return (xs', Nothing)
     where
        applyHint (w,r@(Rectangle a b c d)) = do
            adj <- mkAdjust w
            let (c',d') = adj (c,d)
            return (w, if isInStack s w then Rectangle a b c' d' else r)

-- | @placeRectangle (rx, ry) r0 r@ will return a new rectangle with the same dimensions
-- as @r@, but positioned inside of @r0@ as specified by the (rx, ry) parameters (see
-- 'layoutHintsWithPlacement').
placeRectangle :: RealFrac r => (r, r) -> Rectangle -> Rectangle -> Rectangle
placeRectangle (rx, ry) (Rectangle x0 y0 w h) (Rectangle _ _ dx dy)
    = Rectangle (align x0 dx w rx) (align y0 dy h ry) dx dy
    where align :: RealFrac r => Position -> Dimension -> Dimension -> r -> Position
          align z0 dz d r = z0 + truncate (fromIntegral (d - dz) * r)

fitting :: [Rectangle] -> Int
fitting rects = sum $ do
    r <- rects
    return $ length $ filter (touching r) rects

applyOrder :: Rectangle -> [((Window, Rectangle),t)] -> [[((Window, Rectangle),t)]]
applyOrder root wrs = do
    -- perhaps it would just be better to take all permutations, or apply the
    -- resizing multiple times
    f <- [maximum, minimum, sum, sum . map sq]
    return $ sortBy (compare `on` (f . distance)) wrs
    where distFC = uncurry ((+) `on` sq) . pairWise (-) (center root)
          distance = map distFC . corners . snd . fst
          pairWise f (a,b) (c,d) = (f a c, f b d)
          sq = join (*)

data LayoutHintsToCenter a = LayoutHintsToCenter deriving (Read, Show)

instance LayoutModifier LayoutHintsToCenter Window where
    modifyLayout _ ws@(W.Workspace _ _ Nothing) r = runLayout ws r
    modifyLayout _ ws@(W.Workspace _ _ (Just st)) r = do
        (arrs,ol) <- runLayout ws r
        (, ol) . changeOrder (W.focus st : filter (/= W.focus st) (map fst arrs))
               . maximumBy (compare `on` (fitting . map snd))
               . map (applyHints st r) . applyOrder r
             <$> mapM (\x -> (x,) <$> mkAdjust (fst x)) arrs

changeOrder :: [Window] -> [(Window, Rectangle)] -> [(Window, Rectangle)]
changeOrder w wr = zip w' $ map (fromJust . flip lookup wr) w'
    where w' = filter (`elem` map fst wr) w

-- apply hints to first, grow adjacent windows
applyHints :: W.Stack Window -> Rectangle -> [((Window, Rectangle),D -> D)] -> [(Window, Rectangle)]
applyHints _ _ [] = []
applyHints s root (((w,lrect@(Rectangle a b c d)),adj):xs) =
        let (c',d') = adj (c,d)
            redr = placeRectangle (centerPlacement root lrect :: (Double,Double)) lrect
                    $ if isInStack s w then Rectangle a b c' d' else lrect

            ds = (fromIntegral c - fromIntegral c',fromIntegral d - fromIntegral d')
            growOther' = growOther ds lrect (freeDirs root lrect)
            mapSnd f = map (first $ second f)
            next = applyHints s root $ mapSnd growOther' xs
        in (w,redr):next

growOther :: (Position, Position) -> Rectangle -> Set Direction2D -> Rectangle -> Rectangle
growOther ds lrect fds r
    | dirs <- flipDir <$> Set.toList (Set.intersection adj fds)
    , not $ any (uncurry opposite) $ cross dirs =
        foldr (`grow` ds) r dirs
    | otherwise = r
    where
        adj = adjacent lrect  r
        cross xs = [ (a,b) | a <- xs, b <- xs ]

        flipDir :: Direction2D -> Direction2D
        flipDir d = case d of { L -> R; U -> D; R -> L; D -> U }

        opposite :: Direction2D -> Direction2D -> Bool
        opposite x y = flipDir x == y

-- | Leave the opposite edges where they were
grow :: Direction2D -> (Position,Position) -> Rectangle -> Rectangle
grow L (px,_ ) (Rectangle x y w h) = Rectangle (x-px) y (w+fromIntegral px) h
grow U (_ ,py) (Rectangle x y w h) = Rectangle x (y-py) w (h+fromIntegral py)
grow R (px,_ ) (Rectangle x y w h) = Rectangle x y (w+fromIntegral px) h
grow D (_ ,py) (Rectangle x y w h) = Rectangle x y w (h+fromIntegral py)

comparingEdges :: ([Position] -> [Position] -> Bool) -> Rectangle -> Rectangle -> Set Direction2D
comparingEdges surrounds r1 r2 = Set.fromList $ map fst $ filter snd [ (dir,) $
            any and [[dir `elem` [R,L], allEq [a,c,w,y], [b,d] `surrounds` [x,z]]
                    ,[dir `elem` [U,D], allEq [b,d,x,z], [a,c] `surrounds` [w,y]]]
    | ((a,b),(c,d)) <- edge $ corners r1
    | ((w,x),(y,z)) <- edge $ delay 2 $ corners r2
    | dir <- [U,R,D,L]]
        where edge (x:xs) = zip (x:xs) (xs ++ [x])
              edge [] = []
              delay n xs = drop n xs ++ take n xs
              allEq = all (uncurry (==)) . edge

-- | in what direction is the second window from the first that can expand if the
-- first is shrunk, assuming that the root window is fully covered:
--  one direction for a common edge
--  two directions for a common corner
adjacent :: Rectangle -> Rectangle -> Set Direction2D
adjacent = comparingEdges (all . onClosedInterval)

-- | True whenever two edges touch. not (Set.null $ adjacent x y) ==> touching x y
touching :: Rectangle -> Rectangle -> Bool
touching a b = not . Set.null $ comparingEdges c a b
    where c x y = any (onClosedInterval x) y || any (onClosedInterval y) x

onClosedInterval :: Ord a => [a] -> a -> Bool
onClosedInterval bds x = minimum bds <= x && maximum bds >= x

-- | starting top left going clockwise
corners :: Rectangle -> [(Position, Position)]
corners (Rectangle x y w h) = [(x,y)
                              ,(x+fromIntegral w, y)
                              ,(x+fromIntegral w, y+fromIntegral h)
                              ,(x, y+fromIntegral h)]

center :: Rectangle -> (Position, Position)
center (Rectangle x y w h) = (avg x w, avg y h)
    where avg a b = a + fromIntegral b `div` 2

centerPlacement :: RealFrac r => Rectangle -> Rectangle -> (r, r)
centerPlacement = centerPlacement' clamp
    where clamp n = case signum n of
                            0 -> 0.5
                            1 -> 1
                            _ -> 0

freeDirs :: Rectangle -> Rectangle -> Set Direction2D
freeDirs root = Set.fromList . uncurry (++) . (lr *** ud)
              . centerPlacement' signum root
    where
        lr 1 = [L]
        lr (-1) = [R]
        lr _ = [L,R]
        ud 1 = [U]
        ud (-1) = [D]
        ud _ = [U,D]

centerPlacement' :: (Position -> r) -> Rectangle -> Rectangle -> (r, r)
centerPlacement' cf root assigned
    = (cf $ cx - cwx, cf $ cy - cwy)
    where (cx,cy) = center root
          (cwx,cwy) = center assigned

-- | Event hook that refreshes the layout whenever a window changes its hints.
hintsEventHook :: Event -> X All
hintsEventHook PropertyEvent{ ev_event_type = t, ev_atom = a, ev_window = w }
    | t == propertyNotify && a == wM_NORMAL_HINTS = do
        whenX (isClient w <&&> hintsMismatch w) refresh
        return (All True)
hintsEventHook _ = return (All True)

-- | True if the window's current size does not satisfy its size hints.
hintsMismatch :: Window -> X Bool
hintsMismatch w = safeGetWindowAttributes w >>= \case
    Nothing -> pure False
    Just wa -> do
        sh <- withDisplay $ \d -> io (getWMNormalHints d w)
        let dim = (fromIntegral $ wa_width wa, fromIntegral $ wa_height wa)
        return $ dim /= applySizeHints 0 sh dim
