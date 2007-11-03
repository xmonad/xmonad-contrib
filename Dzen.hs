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

module XMonadContrib.Dzen (dzen, dzenWithArgs, dzenScreen,
                           dzenUrgencyHook, dzenUrgencyHookWithArgs,
                           seconds) where

import Control.Monad (when)
import Control.Monad.State (gets)
import qualified Data.Set as S
import Graphics.X11.Types (Window)

import qualified StackSet as W
import XMonad

import XMonadContrib.NamedWindows (getName)
import XMonadContrib.Run (runProcessWithInputAndWait, seconds)

-- | @dzen str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds.
-- Example usage:
-- > dzen "Hi, mom!" (5 `seconds`)
dzen :: String -> Int -> X ()
dzen str timeout = dzenWithArgs str [] timeout

-- | @dzen str args timeout@ pipes @str@ to dzen2 for @timeout@ seconds, passing @args@ to dzen.
-- Example usage:
-- > dzen "Hi, dons!" ["-ta", "r"] (5 `seconds`)
dzenWithArgs :: String -> [String] -> Int -> X ()
dzenWithArgs str args timeout = io $ runProcessWithInputAndWait "dzen2" args (unchomp str) timeout
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

-- | Flashes when a window requests your attention and you can't see it. For use with
-- XMonadContrib.UrgencyHook. Usage:
-- > urgencyHook = dzenUrgencyHook (5 `seconds`)
dzenUrgencyHook :: Int -> Window -> X ()
dzenUrgencyHook = dzenUrgencyHookWithArgs []

-- | Flashes when a window requests your attention and you can't see it. For use with
-- XMonadContrib.UrgencyHook. Usage:
-- > urgencyHook = dzenUrgencyHook ["-bg", "darkgreen"] (5 `seconds`)
dzenUrgencyHookWithArgs :: [String] -> Int -> Window -> X ()
dzenUrgencyHookWithArgs args duration w = do
    visibles <- gets mapped
    name <- getName w
    ws <- gets windowset
    whenJust (W.findTag w ws) (flash name visibles)
  where flash name visibles index =
              when (not $ S.member w visibles) $
              dzenWithArgs (show name ++ " requests your attention on workspace " ++ index)
                           args duration
