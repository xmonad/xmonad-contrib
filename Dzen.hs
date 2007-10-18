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

module XMonadContrib.Dzen (dzen, dzenScreen, dzenUrgencyHook, seconds) where

import Control.Monad (when)
import Control.Monad.State (gets)
import qualified Data.Set as S
import Graphics.X11.Types (Window)

import qualified StackSet as W
import XMonad

import XMonadContrib.NamedWindows (getName)
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

-- | Flashes when a window requests your attention and you can't see it. For use with
-- XMonadContrib.UrgencyHook. Usage:
-- > urgencyHook = dzenUrgencyHook (5 `seconds`)
-- Bug: Doesn't flash if you're on the same workspace, Full or Tabbed layout, different window.
dzenUrgencyHook :: Int -> Window -> X ()
dzenUrgencyHook duration w = do
    visibles <- gets mapped
    name <- getName w
    ws <- gets windowset
    whenJust (W.findIndex w ws) (flash name ws visibles)
  where flash name ws visibles index =
              when (index /= W.tag (W.workspace (W.current ws)) && not (S.member w visibles)) $
              dzen (show name ++ " requests your attention on workspace " ++ index) duration

dzenWithArgs :: String -> [String] -> Int -> X ()
dzenWithArgs str args timeout = io $ runProcessWithInputAndWait "dzen2" args (unchomp str) timeout
  -- dzen seems to require the input to terminate with exactly one newline.
  where unchomp s@['\n'] = s
        unchomp []       = ['\n']
        unchomp (c:cs)   = c : unchomp cs
