-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RandomBackground
-- Description :  Start terminals with a random background color.
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

module XMonad.Actions.RandomBackground (
    -- * Usage
    -- $usage
    randomBg',
    randomBg,
    RandomColor(HSV,RGB)
    ) where

import XMonad(X, XConf(config), XConfig(terminal), io, spawn,
              MonadIO, asks)
import System.Random
import Numeric(showHex)

-- $usage
--
-- Add to your keybindings something like:
--
-- > ,((modm .|. shiftMask, xK_Return), randomBg $ HSV 0xff 0x20

-- | RandomColor fixes constraints when generating random colors. All
-- parameters should be in the range 0 -- 0xff
data RandomColor = RGB { _colorMin :: Int
                       , _colorMax :: Int
                       } -- ^ specify the minimum and maximum lowest values for each color channel.
                 | HSV { _colorSaturation :: Double
                       , _colorValue :: Double
                       } -- ^ specify the saturation and value, leaving the hue random.

toHex :: [Int] -> String
toHex =  ("'#"++) . (++"'") . concatMap (ensure 2 . ($ "") . showHex)
    where ensure x = reverse . take x . (++repeat '0') . reverse

randPermutation ::  (RandomGen g) => [a] -> g -> [a]
randPermutation xs g = swap $ zip (randoms g) xs
  where
    swap ((True,x):(c,y):ys) = y:swap ((c,x):ys)
    swap ((False,x):ys) = x:swap ys
    swap x = map snd x

-- | @randomBg'@ produces a random hex number in the form @'#xxyyzz'@
randomBg' ::  (MonadIO m) => RandomColor -> m String
randomBg' (RGB l h) = io $ fmap (toHex . take 3 . randomRs (l,h)) newStdGen
randomBg' (HSV s v) = io $ do
    g <- newStdGen
    let x = (^(2::Int)) $ fst $ randomR (0,sqrt $ pi / 3) g
    return $ toHex $ map round $ randPermutation [v,(v-s)*x + s,s] g

-- | @randomBg@ starts a terminal with the background color taken from 'randomBg''
--
-- This depends on the your 'terminal' configuration field accepting an
-- argument like @-bg '#ff0023'@
randomBg :: RandomColor -> X ()
randomBg x = do
    t <- asks (terminal . config)
    c <- randomBg' x
    spawn $ t ++ " -bg " ++ c
