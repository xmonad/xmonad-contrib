module XMonadContrib.Mosaic ( mosaic, expandWindow, shrinkWindow, squareWindow ) where

-- This module defines a "mosaic" layout, which tries to give all windows
-- equal area, while also trying to give them a user-defined (and run-time
-- adjustable) aspect ratio.  You can use mod-l and mod-h to adjust the
-- aspect ratio (which probably won't have a very interesting effect unless
-- you've got a number of windows upen.

-- My intent is to extend this layout to optimize various constraints, such
-- as windows that should have a different aspect ratio, a fixed size, or
-- minimum dimensions in certain directions.

-- You can use this module with the following in your config file:

-- import XMonadContrib.Mosaic
-- import Control.Monad.State ( gets )
-- import qualified StackSet as W ( peek )

-- defaultLayouts :: [Layout]
-- defaultLayouts = [ mosaic (1%4) (1%2) M.empty M.empty, full,
--                    tall defaultDelta (1%2), wide defaultDelta (1%2) ]

-- In the key-bindings, do something like:

--     , ((modMask .|. shiftMask, xK_h     ), do ws <- gets workspace
--                                               whenJust (W.peek ws) $ \w ->
--                                                   layoutMsg (shrinkWindow w))
--     , ((modMask .|. shiftMask, xK_l     ), do ws <- gets workspace
--                                               whenJust (W.peek ws) $ \w ->
--                                                   layoutMsg (expandWindow w))
--     , ((modMask .|. shiftMask, xK_s     ), do ws <- gets workspace
--                                               whenJust (W.peek ws) $ \w ->
--                                                   layoutMsg (squareWindow w))

import Data.Ratio
import Graphics.X11.Xlib
import XMonad
import Operations ( ShrinkOrExpand (Shrink, Expand) )
import qualified Data.Map as M
import Data.List ( sort )
import Data.Dynamic ( Typeable, fromDynamic )
import Control.Monad ( mplus )

import System.IO.Unsafe

data HandleWindow = ExpandWindow Window | ShrinkWindow Window | SquareWindow Window
                    deriving ( Typeable, Eq )
expandWindow, shrinkWindow, squareWindow :: Window -> HandleWindow
expandWindow = ExpandWindow
shrinkWindow = ShrinkWindow
squareWindow = SquareWindow

mosaic :: Rational -> Rational -> M.Map Window WindowRater -> M.Map Window Area -> Layout
mosaic delta tileFrac raters areas = Layout { doLayout = mosaicL tileFrac raters areas
                                            , modifyLayout = mlayout }
    where mlayout x = (m1 `fmap` fromDynamic x) `mplus` (m2 `fmap` fromDynamic x)
          m1 Shrink = mosaic delta (tileFrac/(1+delta)) raters areas
          m1 Expand = mosaic delta (tileFrac*(1+delta)) raters areas
          m2 (ExpandWindow w) = mosaic delta tileFrac raters
                                -- (add_rater (\_ (Rectangle _ _ wid h) -> delta*(1-wid///h)) w raters)
                                (multiply_area (1+delta) w areas)
          m2 (ShrinkWindow w) = mosaic delta tileFrac raters
                                -- (add_rater (\_ (Rectangle _ _ wid h) -> delta*(wid///h-1)) w raters)
                                (multiply_area (1/(1+ delta)) w areas)
          m2 (SquareWindow w) = mosaic delta tileFrac (M.insert w force_square raters) areas
          force_square _ (Rectangle _ _ a b) = 100*(sqr(a///b) + sqr(b///a))
          sqr a = a * a

mytrace :: String -> a -> a
mytrace s a = seq foo a
    where foo = unsafePerformIO $ appendFile "/tmp/xmonad.trace" (s++"\n")

myerror :: String -> a
myerror s = seq foo $ error s
    where foo = unsafePerformIO $ appendFile "/tmp/xmonad.trace" (s++"\n")

multiply_area :: Area -> Window -> M.Map Window Area -> M.Map Window Area
multiply_area a w = M.alter (Just . f) w where f Nothing = a
                                               f (Just a') = a'*a

add_rater :: WindowRater -> Window -> M.Map Window WindowRater -> M.Map Window WindowRater
add_rater r w = M.alter f w where f Nothing= Just r
                                  f (Just r') = Just $ \foo bar -> r foo bar + r' foo bar

type WindowRater = Window -> Rectangle -> Rational

mosaicL :: Rational -> M.Map Window WindowRater -> M.Map Window Area
        -> Rectangle -> [Window] -> [(Window, Rectangle)]
mosaicL _ _ _ _ [] = []
mosaicL f raters areas origRect origws
    = flattenMosaic $ the_value $ if myv < myh then myv else myh
    where mean_area = area origRect / fromIntegral (length origws)
          myv = my_mosaic origRect Vertical sortedws
          myh = my_mosaic origRect Horizontal sortedws
          sortedws = reverse $ map the_value $ sort $ map (\w -> Rated (sumareas [w]) w) origws

          my_mosaic :: Rectangle -> CutDirection -> [Window]
                    -> Rated Rational (Mosaic (Window, Rectangle))
          my_mosaic _ _ [] = Rated 0 $ M []
          my_mosaic r _ [w] = Rated (rating w r) $ OM (w,r)
          my_mosaic r d ws = minL $
                             map (fmap M . catRated .
                                  map (\(ws',r') -> my_mosaic r' (otherDirection d) ws')) $
                             map (\ws' -> zip ws' $ partitionR d r $ map sumareas ws') $
                             init $ allsplits ws
              where minL [] = myerror "minL on empty list"
                    minL [a] = a
                    minL (a:b:c) = minL (min a b:c)

          partitionR :: CutDirection -> Rectangle -> [Area] -> [Rectangle]
          partitionR _ _ [] = []
          partitionR _ r [_] = [r]
          partitionR d r (a:ars) = r1 : partitionR d r2 ars
              where totarea = sum (a:ars)
                    (r1,r2) = split d (a/totarea) r

          rating :: WindowRater
          rating w r = (M.findWithDefault default_preferences w raters) w r
          default_preferences :: WindowRater
          default_preferences _ r@(Rectangle _ _ w h)
              | fr <- w /// h = sqr(fr/f)+sqr(f/fr)-2+ toRational (mean_area/area r)
          sqr a = a * a
          sumareas ws = sum $ map (\w -> M.findWithDefault 1 w areas) ws



catRated :: Num v => [Rated v a] -> Rated v [a]
catRated xs = Rated (sum $ map the_rating xs) (map the_value xs)

data Rated a b = Rated !a !b
instance Functor (Rated a) where
    f `fmap` (Rated v a) = Rated v (f a)

the_value :: Rated a b -> b
the_value (Rated _ b) = b
the_rating :: Rated a b -> a
the_rating (Rated a _) = a

instance Eq a => Eq (Rated a b) where
    (Rated a _) == (Rated a' _) = a == a'
instance Ord a => Ord (Rated a b) where
    compare (Rated a _) (Rated a' _) = compare a a'

type Area = Rational

area :: Rectangle -> Area
area (Rectangle _ _ w h) = fromIntegral w * fromIntegral h

(///) :: (Integral a, Integral b) => a -> b -> Rational
a /// b = fromIntegral a / fromIntegral b


split :: CutDirection -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Vertical frac (Rectangle sx sy sw sh) = (Rectangle sx sy sw h,
                                               Rectangle sx (sy+fromIntegral h) sw (sh-h))
    where h = floor $ fromIntegral sh * frac
split Horizontal frac (Rectangle sx sy sw sh) = (Rectangle sx sy w sh,
                                                 Rectangle (sx+fromIntegral w) sy (sw-w) sh)
    where w = floor $ fromIntegral sw * frac

data CutDirection = Vertical | Horizontal
otherDirection :: CutDirection -> CutDirection
otherDirection Vertical = Horizontal
otherDirection Horizontal = Vertical

data Mosaic a where
    M :: [Mosaic a] -> Mosaic a
    OM :: a -> Mosaic a

flattenMosaic :: Mosaic a -> [a]
flattenMosaic (OM a) = [a]
flattenMosaic (M xs) = concatMap flattenMosaic xs

allsplits :: [a] -> [[[a]]]
allsplits [] = [[[]]]
allsplits [a] = [[[a]]]
allsplits (x:xs) = (map ([x]:) splitsrest) ++
                   (map (maphead (x:)) splitsrest)
    where splitsrest = allsplits xs

maphead :: (a->a) -> [a] -> [a]
maphead f (x:xs) = f x : xs
maphead _ [] = []
