{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Mosaic
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module defines a \"mosaic\" layout, which tries to give each window a
-- user-configurable relative area, while also trying to give them aspect
-- ratios configurable at run-time by the user.
--
-----------------------------------------------------------------------------
module XMonadContrib.Mosaic (
                             -- * Usage
                             -- $usage
                             mosaic, expandWindow, shrinkWindow, squareWindow, myclearWindow,
                             tallWindow, wideWindow, flexibleWindow,
                             getName, withNamedWindow ) where

import Control.Monad.State ( State, put, get, runState )
import System.Random ( StdGen, mkStdGen )

import Data.Ratio
import Graphics.X11.Xlib
import XMonad hiding ( trace )
import Operations ( full, Resize(Shrink, Expand) )
import qualified StackSet as W
import qualified Data.Map as M
import Data.List ( sort )
import Data.Typeable ( Typeable )
import Control.Monad ( mplus )

import XMonadContrib.NamedWindows
import XMonadContrib.Anneal

-- $usage
--
-- Key bindings:
-- 
-- You can use this module with the following in your config file:
--
-- > import XMonadContrib.Mosaic
--
-- > defaultLayouts :: [Layout]
-- > defaultLayouts = [ mosaic 0.25 0.5 M.empty M.empty, full,
-- >                    tall defaultDelta (1%2), wide defaultDelta (1%2) ]
--
-- In the key-bindings, do something like:
--
-- >   , ((controlMask .|. modMask .|. shiftMask, xK_h), withNamedWindow (sendMessage . tallWindow))
-- >   , ((controlMask .|. modMask .|. shiftMask, xK_l), withNamedWindow (sendMessage . wideWindow))
-- >   , ((modMask .|. shiftMask, xK_h     ), withNamedWindow (sendMessage . shrinkWindow))
-- >   , ((modMask .|. shiftMask, xK_l     ), withNamedWindow (sendMessage . expandWindow))
-- >   , ((modMask .|. shiftMask, xK_s     ), withNamedWindow (sendMessage . squareWindow))
-- >   , ((modMask .|. shiftMask, xK_o     ), withNamedWindow (sendMessage . myclearWindow))
-- >   , ((controlMask .|. modMask .|. shiftMask, xK_o     ), withNamedWindow (sendMessage . flexibleWindow))
-- 

data HandleWindow = ExpandWindow NamedWindow | ShrinkWindow NamedWindow
                  | SquareWindow NamedWindow | ClearWindow NamedWindow
                  | TallWindow NamedWindow | WideWindow NamedWindow
                  | FlexibleWindow NamedWindow
                    deriving ( Typeable, Eq )

instance Message HandleWindow

expandWindow, shrinkWindow, squareWindow, flexibleWindow, myclearWindow,tallWindow, wideWindow :: NamedWindow -> HandleWindow
expandWindow = ExpandWindow
shrinkWindow = ShrinkWindow
squareWindow = SquareWindow
flexibleWindow = FlexibleWindow
myclearWindow = ClearWindow
tallWindow = TallWindow
wideWindow = WideWindow

largeNumber :: Int
largeNumber = 50

defaultArea :: Double
defaultArea = 1

flexibility :: Double
flexibility = 0.1

mosaic :: Double -> Double -> M.Map NamedWindow [WindowHint] -> Layout Window
mosaic delta tileFrac hints = full { doLayout = \r -> mosaicL tileFrac hints r . W.integrate, modifyLayout = return . mlayout }
    where mlayout x = (m1 `fmap` fromMessage x) `mplus` (m2 `fmap` fromMessage x)
          m1 Shrink = mosaic delta (tileFrac/(1+delta)) hints
          m1 Expand = mosaic delta (tileFrac*(1+delta)) hints
          m2 (ExpandWindow w) = mosaic delta tileFrac (multiply_area (1+delta) w hints)
          m2 (ShrinkWindow w) = mosaic delta tileFrac (multiply_area (1/(1+ delta)) w hints)
          m2 (SquareWindow w) = mosaic delta tileFrac (set_aspect_ratio 1 w hints)
          m2 (FlexibleWindow w) = mosaic delta tileFrac (make_flexible w hints)
          m2 (TallWindow w) = mosaic delta tileFrac (multiply_aspect (1/(1+delta)) w hints)
          m2 (WideWindow w) = mosaic delta tileFrac (multiply_aspect (1+delta) w hints)
          m2 (ClearWindow w) = mosaic delta tileFrac (M.delete w hints)

multiply_area :: Double -> NamedWindow
              -> M.Map NamedWindow [WindowHint] -> M.Map NamedWindow [WindowHint]
multiply_area a = alterlist f where f [] = [RelArea (defaultArea*a)]
                                    f (RelArea a':xs) = RelArea (a'*a) : xs
                                    f (x:xs) = x : f xs

set_aspect_ratio :: Double -> NamedWindow
                 -> M.Map NamedWindow [WindowHint] -> M.Map NamedWindow [WindowHint]
set_aspect_ratio r = alterlist f where f [] = [AspectRatio r]
                                       f (FlexibleAspectRatio _:x) = AspectRatio r:x
                                       f (AspectRatio _:x) = AspectRatio r:x
                                       f (x:xs) = x:f xs

make_flexible :: NamedWindow
              -> M.Map NamedWindow [WindowHint] -> M.Map NamedWindow [WindowHint]
make_flexible = alterlist (map f) where f (AspectRatio r) = FlexibleAspectRatio r
                                        f (FlexibleAspectRatio r) = AspectRatio r
                                        f x = x

multiply_aspect :: Double -> NamedWindow
                -> M.Map NamedWindow [WindowHint] -> M.Map NamedWindow [WindowHint]
multiply_aspect r = alterlist f where f [] = [FlexibleAspectRatio r]
                                      f (AspectRatio r':x) = AspectRatio (r*r'):x
                                      f (FlexibleAspectRatio r':x) = FlexibleAspectRatio (r*r'):x
                                      f (x:xs) = x:f xs

findlist :: Ord k => k -> M.Map k [a] -> [a]
findlist = M.findWithDefault []

alterlist :: (Ord k, Ord a) => ([a] -> [a]) -> k -> M.Map k [a] -> M.Map k [a]
alterlist f k = M.alter f' k
    where f' Nothing = f' (Just [])
          f' (Just xs) = case f xs of
                           [] -> Nothing
                           xs' -> Just xs'

mosaicL :: Double -> M.Map NamedWindow [WindowHint]
        -> Rectangle -> [Window] -> X [(Window, Rectangle)]
mosaicL _ _ _ [] = return []
mosaicL f hints origRect origws
    = do namedws <- mapM getName origws
         let sortedws = reverse $ map the_value $ sort $ map (\w -> Rated (sumareas [w]) w) namedws
             -- TODO: remove all this dead code
             myv = runCountDown largeNumber $ mosaic_splits even_split origRect Vertical sortedws
             myv2 = mc_mosaic sortedws Vertical
             myh2 = mc_mosaic sortedws Horizontal
--             myv2 = maxL $ runCountDown largeNumber $
--                    sequence $ replicate mediumNumber $
--                    mosaic_splits one_split origRect Vertical sortedws
             myh = runCountDown largeNumber $ mosaic_splits even_split origRect Horizontal sortedws
--             myh2 = maxL $ runCountDown largeNumber $
--                    sequence $ replicate mediumNumber $
--                    mosaic_splits one_split origRect Horizontal sortedws
         return $ map (\(nw,r)->(--trace ("rate1:"++ unlines [show nw,
                                 --                           show $ rate f meanarea (findlist nw hints) r,
                                 --                           show r,
                                 --                           show $ area r/meanarea,
                                 --                           show $ findlist nw hints]) $
                                 unName nw,crop' (findlist nw hints) r)) $
                flattenMosaic $ the_value $ maxL [myh,myv,myh2,myv2]
    where mosaic_splits _ _ _ [] = return $ Rated 0 $ M []
          mosaic_splits _ r _ [w] = return $ Rated (rate f meanarea (findlist w hints) r) $ OM (w,r)
          mosaic_splits spl r d ws = maxL `fmap` mapCD (spl r d) (init $ allsplits ws)
          even_split :: Rectangle -> CutDirection -> [[NamedWindow]]
                     -> State CountDown (Rated Double (Mosaic (NamedWindow, Rectangle)))
          even_split r d [ws] = even_split r d $ map (:[]) ws
          even_split r d wss =
              do let areas = map sumareas wss
                 let wsr_s :: [([NamedWindow], Rectangle)]
                     wsr_s = zip wss (partitionR d r areas)
                 submosaics <- mapM (\(ws',r') ->
                                     mosaic_splits even_split r' (otherDirection d) ws') wsr_s
                 return $ fmap M $ catRated submosaics
          {-
          another_mosaic :: [NamedWindow] -> CutDirection
                         -> Rated Double (Mosaic (NamedWindow,Rectangle))
          another_mosaic ws d = rate_mosaic ratew $
                                rect_mosaic origRect d $
                                zipML (example_mosaic ws) (map findarea ws)
          -}
          mc_mosaic :: [NamedWindow] -> CutDirection
                         -> Rated Double (Mosaic (NamedWindow,Rectangle))
          mc_mosaic ws d = fmap (rect_mosaic origRect d) $
                           annealMax (zipML (example_mosaic ws) (map findarea ws))
                           (the_rating . rate_mosaic ratew . rect_mosaic origRect d )
                           changeMosaic

          ratew :: (NamedWindow,Rectangle) -> Double
          ratew (w,r) = rate f meanarea (findlist w hints) r
          example_mosaic :: [NamedWindow] -> Mosaic NamedWindow
          example_mosaic ws = M (map OM ws)
          rect_mosaic :: Rectangle -> CutDirection -> Mosaic (a,Double) -> Mosaic (a,Rectangle)
          rect_mosaic r _ (OM (w,_)) = OM (w,r)
          rect_mosaic r d (M ws) = M $ zipWith (\w' r' -> rect_mosaic r' d' w') ws rs
              where areas = map (sum . map snd . flattenMosaic) ws
                    rs = partitionR d r areas
                    d' = otherDirection d
          rate_mosaic :: ((NamedWindow,Rectangle) -> Double)
                      -> Mosaic (NamedWindow,Rectangle) -> Rated Double (Mosaic (NamedWindow,Rectangle))
          rate_mosaic r m = catRatedM $ fmap (\x -> Rated (r x) x) m
{-
          one_split :: Rectangle -> CutDirection -> [[NamedWindow]]
                    -> State CountDown (Rated Double (Mosaic (NamedWindow, Rectangle)))
          one_split r d [ws] = one_split r d $ map (:[]) ws
          one_split r d wss =
              do rnd <- mapM (const (fractional resolutionNumber)) [1..length wss]
                 let wsr_s :: [([NamedWindow], Rectangle)]
                     wsr_s = zip wss (partitionR d r rnd)
                 submosaics <- mapM (\(ws',r') ->
                                     mosaic_splits even_split r' (otherDirection d) ws') wsr_s
                 return $ fmap M $ catRated submosaics
-}
          partitionR :: CutDirection -> Rectangle -> [Double] -> [Rectangle]
          partitionR _ _ [] = []
          partitionR _ r [_] = [r]
          partitionR d r (a:ars) = r1 : partitionR d r2 ars
              where totarea = sum (a:ars)
                    (r1,r2) = split d (a/totarea) r
          theareas = hints2area `fmap` hints
          sumareas ws = sum $ map findarea ws
          findarea :: NamedWindow -> Double
          findarea w = M.findWithDefault 1 w theareas
          meanarea =  area origRect / fromIntegral (length origws)

maxL :: Ord a => [a] -> a
maxL [] = error "maxL on empty list"
maxL [a] = a
maxL (a:b:c) = maxL (max a b:c)

catRated :: Floating v => [Rated v a] -> Rated v [a]
catRated xs = Rated (product $ map the_rating xs) (map the_value xs)

catRatedM :: Floating v => Mosaic (Rated v a) -> Rated v (Mosaic a)
catRatedM (OM (Rated v x)) =  Rated v (OM x)
catRatedM (M xs) = case catRated $ map catRatedM xs of Rated v xs' -> Rated v (M xs')

data CountDown = CD !StdGen !Int

tries_left :: State CountDown Int
tries_left = do CD _ n <- get
                return (max 0 n)

mapCD :: (a -> State CountDown b) -> [a] -> State CountDown [b]
mapCD f xs = do n <- tries_left
                let len = length xs
                mapM (run_with_only ((n `div` len)+1) . f) $ take (n+1) xs

run_with_only :: Int -> State CountDown a -> State CountDown a
run_with_only limit j =
    do CD g n <- get
       let leftover = n - limit
       if leftover < 0 then j
                       else do put $ CD g limit
                               x <- j
                               CD g' n' <- get
                               put $ CD g' (leftover + n')
                               return x

data WindowHint = RelArea Double
                | AspectRatio Double
                | FlexibleAspectRatio Double
                  deriving ( Show, Read, Eq, Ord )

fixedAspect :: [WindowHint] -> Bool
fixedAspect [] = False
fixedAspect (AspectRatio _:_) = True
fixedAspect (_:x) = fixedAspect x

rate :: Double -> Double -> [WindowHint] -> Rectangle -> Double
rate defaulta meanarea xs rr
    | fixedAspect xs = (area (crop xs rr) / meanarea) ** weight
    | otherwise = (area rr / meanarea)**(weight-flexibility)
                  * (area (crop (xs++[FlexibleAspectRatio defaulta]) rr) / meanarea)**flexibility
     where weight = hints2area xs

crop :: [WindowHint] -> Rectangle -> Rectangle
crop (AspectRatio f:_) = cropit f
crop (FlexibleAspectRatio f:_) = cropit f
crop (_:hs) = crop hs
crop [] = id

crop' :: [WindowHint] -> Rectangle -> Rectangle
crop' (AspectRatio f:_) = cropit f
crop' (_:hs) = crop' hs
crop' [] = id

cropit :: Double -> Rectangle -> Rectangle
cropit f (Rectangle a b w h) | w -/- h > f = Rectangle a b (ceiling $ h -* f) h
                             | otherwise = Rectangle a b w (ceiling $ w -/ f)

hints2area :: [WindowHint] -> Double
hints2area [] = defaultArea
hints2area (RelArea r:_) = r
hints2area (_:x) = hints2area x

area :: Rectangle -> Double
area (Rectangle _ _ w h) = fromIntegral w * fromIntegral h

(-/-) :: (Integral a, Integral b) => a -> b -> Double
a -/- b = fromIntegral a / fromIntegral b

(-/) :: (Integral a) => a -> Double -> Double
a -/ b = fromIntegral a / b

(-*) :: (Integral a) => a -> Double -> Double
a -* b = fromIntegral a * b

split :: CutDirection -> Double -> Rectangle -> (Rectangle, Rectangle)
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

data Mosaic a = M [Mosaic a] | OM a
          deriving ( Show )

instance Functor Mosaic where
    fmap f (OM x) = OM (f x)
    fmap f (M xs) = M (map (fmap f) xs)

zipMLwith :: (a -> b -> c) -> Mosaic a -> [b] -> Mosaic c
zipMLwith f (OM x) (y:_) = OM (f x y)
zipMLwith _ (OM _) [] = error "bad zipMLwith"
zipMLwith f (M xxs) yys = makeM $ foo xxs yys
    where foo (x:xs) ys = zipMLwith f x (take (lengthM x) ys) :
                          foo xs (drop (lengthM x) ys)
          foo [] _ = []

zipML :: Mosaic a -> [b] -> Mosaic (a,b)
zipML = zipMLwith (\a b -> (a,b))

lengthM :: Mosaic a -> Int
lengthM (OM _) = 1
lengthM (M x) = sum $ map lengthM x

changeMosaic :: Mosaic a -> [Mosaic a]
changeMosaic (OM _) = []
changeMosaic (M xs) = map makeM (concatenations xs) ++
                      map makeM (splits xs) ++
                      map M (tryAll changeMosaic xs)

tryAll :: (a -> [a]) -> [a] -> [[a]]
tryAll _ [] = []
tryAll f (x:xs) = map (:xs) (f x) ++ map (x:) (tryAll f xs)

splits :: [Mosaic a] -> [[Mosaic a]]
splits [] = []
splits (OM x:y) = map (OM x:) $ splits y
splits (M (x:y):z) = (x:makeM y:z) : map (makeM (x:y) :) (splits z)
splits (M []:x) = splits x

concatenations :: [Mosaic a] -> [[Mosaic a]]
concatenations (x:y:z) = (concatenateMosaic x y:z):(map (x:) $ concatenations (y:z))
concatenations _ = []

concatenateMosaic :: Mosaic a -> Mosaic a -> Mosaic a
concatenateMosaic (OM a) (OM b) = M [OM a, OM b]
concatenateMosaic (OM a) (M b) = M (OM a:b)
concatenateMosaic (M a) (OM b) = M (a++[OM b])
concatenateMosaic (M a) (M b) = M (a++b)

makeM :: [Mosaic a] -> Mosaic a
makeM [m] = m
makeM [] = error "makeM []"
makeM ms = M ms

flattenMosaic :: Mosaic a -> [a]
flattenMosaic (OM a) = [a]
flattenMosaic (M xs) = concatMap flattenMosaic xs

allsplits :: [a] -> [[[a]]]
allsplits [] = [[[]]]
allsplits [a] = [[[a]]]
allsplits (x:xs) = (map ([x]:) splitsrest) ++ (map (maphead (x:)) splitsrest)
    where splitsrest = allsplits' xs

allsplits' :: [a] -> [[[a]]]
allsplits' [] = [[[]]]
allsplits' [a] = [[[a]]]
allsplits' (x:xs) = (map (maphead (x:)) splitsrest) ++ (map ([x]:) splitsrest)
    where splitsrest = allsplits xs

maphead :: (a->a) -> [a] -> [a]
maphead f (x:xs) = f x : xs
maphead _ [] = []

runCountDown :: Int -> State CountDown a -> a
runCountDown n x = fst $ runState x (CD (mkStdGen n) n)
