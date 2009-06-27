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

module XMonad.Actions.RandomBackground (randomBg,randomBg') where

import XMonad(X, XConf(config), XConfig(terminal), io, spawn,
              MonadIO, asks)
import System.Random(Random(randomRIO))
import Control.Monad(replicateM)
import Numeric(showHex)

-- | randomHex produces hex values in the form @xxyyzz@, with each of @xx@,
-- @yy@, @zz@ within the range specified. The first parameter determines the
-- the number of such groups.
randomHex ::  Int -> (Int, Int) -> IO String
randomHex n = fmap disp . replicateM n . randomRIO
    where ensure x = reverse . take x . (++repeat '0') . reverse
          disp = concatMap $ ensure 2 . ($ "") . showHex

-- | randomBg' appends the random hex @ -bg '#xxyyzz'@ to the supplied string
randomBg' ::  (MonadIO m) => (Int, Int) -> String -> m String
randomBg' x t = do
  num <- io $ randomHex 3 x
  return $ concat [t," -bg '#",num,"'"]

randomBg :: (Int,Int) -> X ()
randomBg x = spawn =<< randomBg' x =<< asks (terminal . config)
