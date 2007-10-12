-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Dzen
-- Copyright   :  (c) glasser@mit.edu
-- License     :  BSD
--
-- Maintainer  :  glasser@mit.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- Handy wrapper for dzen.
--
-----------------------------------------------------------------------------

module XMonadContrib.Dzen (dzen, dzenScreen) where

import Control.Monad.State
import qualified StackSet as W
import XMonad
import XMonadContrib.Run

curScreen :: X ScreenId
curScreen =  (W.screen . W.current) `liftM` gets windowset

toXineramaArg :: ScreenId -> String
toXineramaArg n = show ( ((fromIntegral n)+1)::Int )

-- Requires dzen >= 0.2.4.

dzen :: String -> X ()
dzen str = curScreen >>= \sc -> dzenScreen sc str

dzenScreen :: ScreenId -> String -> X()
dzenScreen sc str = io $ (runProcessWithInputAndWait "dzen2" ["-xs", screen] str 5000000)
    where screen = toXineramaArg sc
