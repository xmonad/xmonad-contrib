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
-- Handy wrapper for dzen. Requires dzen >= 0.2.4.
--
-----------------------------------------------------------------------------

module XMonadContrib.Dzen (dzen, dzenScreen, seconds) where

import XMonad
import XMonadContrib.Run (runProcessWithInputAndWait, seconds)

toXineramaArg :: ScreenId -> String
toXineramaArg n = show ( ((fromIntegral n)+1)::Int )

-- | @dzen str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds.
-- Example usage:
-- > dzen "Hi, mom!" (5 `seconds`)
dzen :: String -> Int -> X ()
dzen str timeout = dzenWithArgs str [] timeout

-- | @dzenScreen sc str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds, and on screen @sc@.
-- Requires dzen to be compiled with Xinerama support.
dzenScreen :: ScreenId -> String -> Int -> X()
dzenScreen sc str timeout = dzenWithArgs str ["-xs", screen] timeout
    where screen  = toXineramaArg sc

dzenWithArgs :: String -> [String] -> Int -> X ()
dzenWithArgs str args timeout = io $ runProcessWithInputAndWait "dzen2" args (unchomp str) timeout
  -- dzen seems to require the input to terminate with exactly one newline.
  where unchomp s@['\n'] = s
        unchomp []       = ['\n']
        unchomp (c:cs)   = c : unchomp cs
