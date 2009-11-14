-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Dzen
-- Copyright   :  (c) glasser@mit.edu
-- License     :  BSD
--
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  unportable
--
-- Handy wrapper for dzen. Requires dzen >= 0.2.4.
--
-----------------------------------------------------------------------------

module XMonad.Util.Dzen (
    dzen,
    dzenWithArgs,
    dzenScreen,
    seconds
  ) where

import XMonad
import XMonad.Util.Run (runProcessWithInputAndWait, seconds)

-- | @dzen str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds.
-- Example usage:
--
-- > dzen "Hi, mom!" (5 `seconds`)
dzen :: String -> Int -> X ()
dzen str timeout = dzenWithArgs str [] timeout

-- | @dzen str args timeout@ pipes @str@ to dzen2 for @timeout@ seconds, passing @args@ to dzen.
-- Example usage:
--
-- > dzenWithArgs "Hi, dons!" ["-ta", "r"] (5 `seconds`)
dzenWithArgs :: String -> [String] -> Int -> X ()
dzenWithArgs str args timeout = runProcessWithInputAndWait "dzen2" args (unchomp str) timeout
  -- dzen seems to require the input to terminate with exactly one newline.
  where unchomp s@['\n'] = s
        unchomp []       = ['\n']
        unchomp (c:cs)   = c : unchomp cs

-- | @dzenScreen sc str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds, and on screen @sc@.
-- Requires dzen to be compiled with Xinerama support.
dzenScreen :: ScreenId -> String -> Int -> X()
dzenScreen sc str timeout = dzenWithArgs str ["-xs", screen] timeout
    where screen  = toXineramaArg sc
          toXineramaArg n = show ( ((fromIntegral n)+1)::Int )
