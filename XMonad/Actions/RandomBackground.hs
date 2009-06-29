-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RandomBackground
-- Copyright   :  (c) 2009 Anze Slosar
--                translation to Haskell by Adam Vogt
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <vogt.adam@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- An action to start terminals with a random background color
--
-----------------------------------------------------------------------------

module XMonad.Actions.RandomBackground (randomBg',randomBg,RandomColor(HSV,RGB)) where

import XMonad(X, XConf(config), XConfig(terminal), io, spawn,
              MonadIO, asks)
import System.Random
import Control.Monad(replicateM,liftM)
import Numeric(showHex)

-- | RandomColor fixes constraints when generating random colors
data RandomColor = RGB { _colorMin :: Int, _colorMax :: Int }
                 | HSV { _colorSaturation :: Double, _colorValue :: Double }

toHex :: [Int] -> String
toHex =  ("'#"++) . (++"'") . concatMap (ensure 2 . ($ "") . showHex)
    where ensure x = reverse . take x . (++repeat '0') . reverse

randPermutation ::  (RandomGen g) => [a] -> g -> [a]
randPermutation xs g = swap $ zip (randoms g) xs
  where
    swap ((True,x):(c,y):ys) = y:swap ((c,x):ys)
    swap ((False,x):ys) = x:swap ys
    swap x = map snd x

-- | randomBg' produces a random hex number in the form @'#xxyyzz'@
randomBg' ::  (MonadIO m) => RandomColor -> m String
randomBg' (RGB l h) = liftM toHex $ replicateM 3 $ io $ randomRIO (l,h)
randomBg' (HSV s v) = io $ do
    g <- newStdGen
    let -- x = (sqrt 3 - tan theta) / sqrt 3
        x = (^2) $ fst $ randomR (0,sqrt $ pi / 3) g
    return $ toHex $ map round $ randPermutation [v,(v-s)*x + s,s] g

randomBg :: RandomColor -> X ()
randomBg x = do
    t <- asks (terminal . config)
    c <- randomBg' x
    spawn $ t ++ " -bg " ++ c
