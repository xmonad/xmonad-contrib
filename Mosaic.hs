module XMonadContrib.Mosaic ( mosaic, expandWindow, shrinkWindow, squareWindow, myclearWindow,
                              getName, withNamedWindow ) where

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

-- defaultLayouts :: [Layout]
-- defaultLayouts = [ mosaic (1%4) (1%2) M.empty M.empty, full,
--                    tall defaultDelta (1%2), wide defaultDelta (1%2) ]

-- In the key-bindings, do something like:

--     , ((modMask .|. shiftMask, xK_h     ), withNamedWindow (sendMessage . shrinkWindow))
--     , ((modMask .|. shiftMask, xK_l     ), withNamedWindow (sendMessage . expandWindow))
--     , ((modMask .|. shiftMask, xK_s     ), withNamedWindow (sendMessage . squareWindow))
--     , ((modMask .|. shiftMask, xK_o     ), withNamedWindow (sendMessage . clearWindow))

import Control.Monad.Reader ( asks )
import Control.Monad.State ( gets )
import qualified StackSet as W ( peek )
import Data.Ratio
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( fetchName )
import XMonad
import Operations ( Resize(Shrink, Expand) )
import qualified Data.Map as M
import Data.List ( sort )
import Data.Typeable ( Typeable )
import Control.Monad ( mplus )

import System.IO.Unsafe

data HandleWindow = ExpandWindow NamedWindow | ShrinkWindow NamedWindow
                  | SquareWindow NamedWindow | ClearWindow NamedWindow
                    deriving ( Typeable, Eq )

instance Message HandleWindow

expandWindow, shrinkWindow, squareWindow, myclearWindow :: NamedWindow -> HandleWindow
expandWindow = ExpandWindow
shrinkWindow = ShrinkWindow
squareWindow = SquareWindow
myclearWindow = ClearWindow

largeNumber :: Int
largeNumber = 100

mosaic :: Rational -> Rational -> M.Map NamedWindow WindowRater -> M.Map NamedWindow Area -> Layout
mosaic delta tileFrac raters areas = Layout { doLayout = mosaicL tileFrac raters areas
                                            , modifyLayout = mlayout }
    where mlayout x = (m1 `fmap` fromMessage x) `mplus` (m2 `fmap` fromMessage x)
          m1 Shrink = mosaic delta (tileFrac/(1+delta)) raters areas
          m1 Expand = mosaic delta (tileFrac*(1+delta)) raters areas
          m2 (ExpandWindow w) = mosaic delta tileFrac raters
                                -- (add_rater (\_ (Rectangle _ _ wid h) -> delta*(1-wid///h)) w raters)
                                (multiply_area (1+delta) w areas)
          m2 (ShrinkWindow w) = mosaic delta tileFrac raters
                                -- (add_rater (\_ (Rectangle _ _ wid h) -> delta*(wid///h-1)) w raters)
                                (multiply_area (1/(1+ delta)) w areas)
          m2 (SquareWindow w) = mosaic delta tileFrac (M.insert w force_square raters) areas
          m2 (ClearWindow w) = mosaic delta tileFrac (M.delete w raters) (M.delete w areas)
          force_square _ (Rectangle _ _ a b) = 100*(sqr(a///b) + sqr(b///a))
          sqr a = a * a

mytrace :: String -> a -> a
mytrace s a = seq foo a
    where foo = unsafePerformIO $ appendFile "/tmp/xmonad.trace" (s++"\n")

myerror :: String -> a
myerror s = seq foo $ error s
    where foo = unsafePerformIO $ appendFile "/tmp/xmonad.trace" (s++"\n")

multiply_area :: Area -> NamedWindow -> M.Map NamedWindow Area -> M.Map NamedWindow Area
multiply_area a w = M.alter (Just . f) w where f Nothing = a
                                               f (Just a') = a'*a

add_rater :: WindowRater -> NamedWindow -> M.Map NamedWindow WindowRater -> M.Map NamedWindow WindowRater
add_rater r w = M.alter f w where f Nothing= Just r
                                  f (Just r') = Just $ \foo bar -> r foo bar + r' foo bar

type WindowRater = NamedWindow -> Rectangle -> Rational

data NamedWindow = NW !String !Window
instance Eq NamedWindow where
    (NW s _) == (NW s' _) = s == s'
instance Ord NamedWindow where
    compare (NW s _) (NW s' _) = compare s s'

mosaicL :: Rational -> M.Map NamedWindow WindowRater -> M.Map NamedWindow Area
        -> Rectangle -> [Window] -> X [(Window, Rectangle)]
mosaicL _ _ _ _ [] = return []
mosaicL f raters areas origRect origws
    = do namedws <- mapM getName origws
         let sortedws = reverse $ map the_value $ sort $ map (\w -> Rated (sumareas [w]) w) namedws
             myv = my_mosaic origRect Vertical sortedws
             myh = my_mosaic origRect Horizontal sortedws
         return $ map (\(nw,r)->(unName nw,r)) $ flattenMosaic $ the_value $ if myv < myh then myv else myh
    where mean_area = area origRect / fromIntegral (length origws)

          my_mosaic :: Rectangle -> CutDirection -> [NamedWindow]
                    -> Rated Rational (Mosaic (NamedWindow, Rectangle))
          my_mosaic _ _ [] = Rated 0 $ M []
          my_mosaic r _ [w] = Rated (rating w r) $ OM (w,r)
          my_mosaic r d ws = minL $
                             map (fmap M . catRated .
                                  map (\(ws',r') -> my_mosaic r' (otherDirection d) ws')) $
                             map (\ws' -> zip ws' $ partitionR d r $ map sumareas ws') $
                             take largeNumber $ init $ allsplits ws
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

getName :: Window -> X NamedWindow
getName w = asks display >>= \d -> do n <- maybe "" id `fmap` io (fetchName d w)
                                      return $ NW n w

unName :: NamedWindow -> Window
unName (NW _ w) = w

withNamedWindow :: (NamedWindow -> X ()) -> X ()
withNamedWindow f = do ws <- gets workspace
                       whenJust (W.peek ws) $ \w -> getName w >>= f
