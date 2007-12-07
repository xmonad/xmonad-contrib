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
module XMonad.Layout.Mosaic (
                             -- * Usage
                             -- $usage
                             mosaic, expandWindow, shrinkWindow, squareWindow, myclearWindow,
                             tallWindow, wideWindow, flexibleWindow,
                             getName ) where

import Control.Monad.State ( State, put, get, runState )
import System.Random ( StdGen, mkStdGen )
import Data.Maybe ( isJust )

import XMonad hiding ( trace )
import XMonad.Layouts ( Resize(Shrink, Expand) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List ( sort )
import Data.Typeable ( Typeable )
import Control.Monad ( mplus )

import XMonad.Util.NamedWindows
import XMonad.Util.Anneal

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Mosaic
-- > import XMonad.Operations
--
-- Then edit your @layoutHook@ by adding the Mosaic layout:
--
-- > myLayouts = mosaic 0.25 0.5 ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >   , ((controlMask .|. modMask x .|. shiftMask, xK_h), withFocused (sendMessage . tallWindow))
-- >   , ((controlMask .|. modMask x .|. shiftMask, xK_l), withFocused (sendMessage . wideWindow))
-- >   , ((modMask x .|. shiftMask, xK_h     ), withFocused (sendMessage . shrinkWindow))
-- >   , ((modMask x .|. shiftMask, xK_l     ), withFocused (sendMessage . expandWindow))
-- >   , ((modMask x .|. shiftMask, xK_s     ), withFocused (sendMessage . squareWindow))
-- >   , ((modMask x .|. shiftMask, xK_o     ), withFocused (sendMessage . myclearWindow))
-- >   , ((controlMask .|. modMask x .|. shiftMask, xK_o     ), withFocused (sendMessage . flexibleWindow))
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data HandleWindow = ExpandWindow Window | ShrinkWindow Window
                  | SquareWindow Window | ClearWindow Window
                  | TallWindow Window | WideWindow Window
                  | FlexibleWindow Window
                    deriving ( Typeable, Eq )

instance Message HandleWindow

expandWindow, shrinkWindow, squareWindow, flexibleWindow, myclearWindow,tallWindow, wideWindow :: Window -> HandleWindow
expandWindow   = ExpandWindow
shrinkWindow   = ShrinkWindow
squareWindow   = SquareWindow
flexibleWindow = FlexibleWindow
myclearWindow  = ClearWindow
tallWindow     = TallWindow
wideWindow     = WideWindow

largeNumber :: Int
largeNumber = 50

defaultArea :: Double
defaultArea = 1

flexibility :: Double
flexibility = 0.1

mosaic :: Double -> Double -> MosaicLayout Window
mosaic d t = Mosaic d t M.empty

data MosaicLayout a = Mosaic Double Double (M.Map Window [WindowHint])
              deriving ( Show, Read )

instance LayoutClass MosaicLayout Window where
    doLayout (Mosaic _ t h) r st = do all_hints <- add_hints (W.integrate st) h
                                      mosaicL t all_hints r (W.integrate st)
        where add_hints [] x = return x
              add_hints (w:ws) x =
                  do z <- withDisplay $ \d -> io $ getWMNormalHints d w
                     let set_asp = case map4 `fmap` sh_aspect z of
                                   Just ((minx,miny),(maxx,maxy))
                                       | or [minx < 1, miny < 1, maxx < 1, maxy < 1] -> id
                                       | minx/miny == maxx/maxy -> set_aspect_ratio (minx/miny) w
                                   _ -> id
                     add_hints ws $ set_MinX z w $ set_MinY z w $ set_MaxX z w $ set_MaxY z w $ set_asp x
              map4 :: Integral a => ((a,a),(a,a)) -> ((Double,Double),(Double,Double))
              map4 ((a,b),(c,d)) = ((fromIntegral a,fromIntegral b),(fromIntegral c,fromIntegral d))

    pureMessage (Mosaic d t h) m = (m1 `fmap` fromMessage m) `mplus` (m2 `fmap` fromMessage m)
        where
          m1 Shrink             = Mosaic d (t/(1+d)) h
          m1 Expand             = Mosaic d (t*(1+d)) h
          m2 (ExpandWindow w)   = Mosaic d t (multiply_area (1+d) w h)
          m2 (ShrinkWindow w)   = Mosaic d t (multiply_area (1/(1+ d)) w h)
          m2 (SquareWindow w)   = Mosaic d t (set_aspect_ratio 1 w h)
          m2 (FlexibleWindow w) = Mosaic d t (make_flexible w h)
          m2 (TallWindow w)     = Mosaic d t (multiply_aspect (1/(1+d)) w h)
          m2 (WideWindow w)     = Mosaic d t (multiply_aspect (1+d) w h)
          m2 (ClearWindow w)    = Mosaic d t (M.delete w h)

    description _ = "mosaic"

multiply_area :: Double -> Window
              -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
multiply_area a = alterlist f where f [] = [RelArea (defaultArea*a)]
                                    f (RelArea a':xs) = RelArea (a'*a) : xs
                                    f (x:xs) = x : f xs

set_aspect_ratio :: Double -> Window
                 -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
set_aspect_ratio r = alterlist f where f [] = [AspectRatio r]
                                       f (FlexibleAspectRatio _:x) = AspectRatio r:x
                                       f (AspectRatio _:x) = AspectRatio r:x
                                       f (x:xs) = x:f xs

make_flexible :: Window
              -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
make_flexible = alterlist (map f) where f (AspectRatio r) = FlexibleAspectRatio r
                                        f (FlexibleAspectRatio r) = AspectRatio r
                                        f x = x

multiply_aspect :: Double -> Window
                -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
multiply_aspect r = alterlist f where f [] = [FlexibleAspectRatio r]
                                      f (AspectRatio r':x) = AspectRatio (r*r'):x
                                      f (FlexibleAspectRatio r':x) = FlexibleAspectRatio (r*r'):x
                                      f (x:xs) = x:f xs

set_MaxX :: SizeHints -> Window -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
set_MaxX h | Just (_,mx) <- sh_max_size h = replaceinmap (isJust . isMaxX) (MaxX $ fromIntegral mx)
           | otherwise = const id

set_MaxY :: SizeHints -> Window -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
set_MaxY h | Just (_,mx) <- sh_max_size h = replaceinmap (isJust . isMaxY) (MaxY $ fromIntegral mx)
           | otherwise = const id

isMaxX,isMaxY :: WindowHint -> Maybe Dimension
isMaxX (MaxX x) = Just x
isMaxX _ = Nothing
isMaxY (MaxY x) = Just x
isMaxY _ = Nothing

set_MinX :: SizeHints -> Window -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
set_MinX h | Just (mx,_) <- sh_min_size h = replaceinmap isMinX (MinX $ fromIntegral mx)
           | otherwise = const id
    where isMinX (MinX _) = True
          isMinX _ = False

set_MinY :: SizeHints -> Window -> M.Map Window [WindowHint] -> M.Map Window [WindowHint]
set_MinY h | Just (_,mx) <- sh_min_size h = replaceinmap isMinY (MinY $ fromIntegral mx)
           | otherwise = const id
    where isMinY (MinY _) = True
          isMinY _ = False

replaceinmap :: Ord a => (a -> Bool) -> a -> Window -> M.Map Window [a] -> M.Map Window [a]
replaceinmap repl v = alterlist f where f [] = [v]
                                        f (x:xs) | repl x = v:xs
                                                 | otherwise = x:f xs

findlist :: Window -> M.Map Window [a] -> [a]
findlist = M.findWithDefault []

alterlist :: (Ord a) => ([a] -> [a]) -> Window -> M.Map Window [a] -> M.Map Window [a]
alterlist f k = M.alter f' k
    where f' Nothing = f' (Just [])
          f' (Just xs) = case f xs of
                           [] -> Nothing
                           xs' -> Just xs'

mosaicL :: Double -> M.Map Window [WindowHint]
        -> Rectangle -> [Window] -> X ([(Window, Rectangle)],Maybe (MosaicLayout Window))
mosaicL _ _ _ [] = return ([], Nothing)
mosaicL f hints origRect origws
    = do let sortedws = reverse $ map the_value $ sort $ map (\w -> Rated (sumareas [w]) w) origws
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
         return (map (\(w,r)->(--trace ("rate1:"++ unlines [show nw,
                                 --                           show $ rate f meanarea (findlist nw hints) r,
                                 --                           show r,
                                 --                           show $ area r/meanarea,
                                 --                           show $ findlist nw hints]) $
                                 w,crop' (findlist w hints) r)) $
                flattenMosaic $ the_value $ maxL [myh,myv,myh2,myv2], Nothing)
    where mosaic_splits _ _ _ [] = return $ Rated 0 $ M []
          mosaic_splits _ r _ [w] = return $ Rated (rate f meanarea (findlist w hints) r) $ OM (w,r)
          mosaic_splits spl r d ws = maxL `fmap` mapCD (spl r d) (init $ allsplits ws)
          even_split :: Rectangle -> CutDirection -> [[Window]]
                     -> State CountDown (Rated Double (Mosaic (Window, Rectangle)))
          even_split r d [ws] = even_split r d $ map (:[]) ws
          even_split r d wss =
              do let areas = map sumareas wss
                     maxds = map (maxd d) wss
                 let wsr_s :: [([Window], Rectangle)]
                     wsr_s = zip wss (partitionR d r maxds areas)
                 submosaics <- mapM (\(ws',r') ->
                                     mosaic_splits even_split r' (otherDirection d) ws') wsr_s
                 return $ fmap M $ catRated submosaics
          {-
          another_mosaic :: [Window] -> CutDirection
                         -> Rated Double (Mosaic (Window,Rectangle))
          another_mosaic ws d = rate_mosaic ratew $
                                rect_mosaic origRect d $
                                zipML (example_mosaic ws) (map findarea ws)
          -}
          mc_mosaic :: [Window] -> CutDirection
                    -> Rated Double (Mosaic (Window,Rectangle))
          mc_mosaic ws d = fmap (rect_mosaic origRect d) $
                           annealMax (zipML (example_mosaic ws) (map findarea ws))
                           (the_rating . rate_mosaic ratew . rect_mosaic origRect d )
                           changeMosaic

          ratew :: (Window,Rectangle) -> Double
          ratew (w,r) = rate f meanarea (findlist w hints) r
          example_mosaic :: [Window] -> Mosaic Window
          example_mosaic ws = M (map OM ws)
          rect_mosaic :: Rectangle -> CutDirection -> Mosaic (a,Double) -> Mosaic (a,Rectangle)
          rect_mosaic r _ (OM (w,_)) = OM (w,r)
          rect_mosaic r d (M ws) = M $ zipWith (\w' r' -> rect_mosaic r' d' w') ws rs
              where areas = map (sum . map snd . flattenMosaic) ws
                    maxds = repeat 1
                    rs = partitionR d r maxds areas
                    d' = otherDirection d
          rate_mosaic :: ((Window,Rectangle) -> Double)
                      -> Mosaic (Window,Rectangle) -> Rated Double (Mosaic (Window,Rectangle))
          rate_mosaic r m = catRatedM $ fmap (\x -> Rated (r x) x) m
{-
          one_split :: Rectangle -> CutDirection -> [[Window]]
                    -> State CountDown (Rated Double (Mosaic (Window, Rectangle)))
          one_split r d [ws] = one_split r d $ map (:[]) ws
          one_split r d wss =
              do rnd <- mapM (const (fractional resolutionNumber)) [1..length wss]
                 let wsr_s :: [([Window], Rectangle)]
                     wsr_s = zip wss (partitionR d r rnd)
                 submosaics <- mapM (\(ws',r') ->
                                     mosaic_splits even_split r' (otherDirection d) ws') wsr_s
                 return $ fmap M $ catRated submosaics
-}
          partitionR :: CutDirection -> Rectangle -> [Dimension] -> [Double] -> [Rectangle]
          partitionR _ _ _ [] = []
          partitionR _ _ [] _ = []
          partitionR _ r _ [_] = [r]
          partitionR d r (m:ms) (a:ars) = r1 : partitionR d r2 ms ars
              where totarea = sum (a:ars)
                    totd = fromIntegral $ dimR d r
                    (r1,r2) = if a/totarea > fromIntegral m / totd
                              then if a/totarea > 1 - fromIntegral (sum ms) / totd
                                   then split d (1 - fromIntegral (sum ms) / totd) r
                                   else split d (a/totarea) r
                              else split d (fromIntegral m / totd) r
          theareas = hints2area `fmap` hints
          sumareas ws = sum $ map findarea ws
          maxd Vertical ws = maximum $ map (findhinted isMaxY 3) ws
          maxd Horizontal ws = maximum $ map (findhinted isMaxX 3) ws
          findarea :: Window -> Double
          findarea w = M.findWithDefault 1 w theareas
          findhinted fh d w = fh' $ M.findWithDefault [] w hints
              where fh' [] = d
                    fh' (h:hs) | Just x <- fh h = x
                               | otherwise = fh' hs
          meanarea =  area origRect / fromIntegral (length origws)

dimR :: CutDirection -> Rectangle -> Dimension
dimR Vertical (Rectangle _ _ _ h) = h
dimR Horizontal (Rectangle _ _ w _) = w

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
                | MaxX Dimension
                | MaxY Dimension
                | MinX Dimension
                | MinY Dimension
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

crop1 :: WindowHint -> Rectangle -> Rectangle
crop1 (FlexibleAspectRatio f) r = cropit f r
crop1 h r = crop1' h r

crop1' :: WindowHint -> Rectangle -> Rectangle
crop1' (AspectRatio f) r = cropit f r
crop1' (FlexibleAspectRatio f) r = cropit f r
crop1' (MaxX xm) (Rectangle x y w h) | w > xm = Rectangle x y xm h
                                     | otherwise = Rectangle x y w h
crop1' (MaxY xm) (Rectangle x y w h) | h > xm = Rectangle x y w xm
                                     | otherwise = Rectangle x y w h
crop1' _ r = r

crop :: [WindowHint] -> Rectangle -> Rectangle
crop (h:hs) = crop hs . crop1 h
crop [] = id

crop' :: [WindowHint] -> Rectangle -> Rectangle
crop' (h:hs) = crop' hs . crop1' h
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
split d frac r | frac <= 0 || frac >= 1 = split d 0.5 r
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
