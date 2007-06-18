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

import System.Posix.Process (forkProcess, getProcessStatus, createSession)
import System.IO
import System.Process
import System.Exit
import Control.Concurrent (threadDelay)
import Control.Monad.State

import qualified StackSet as W
import XMonad

-- wait is in us
runProcessWithInputAndWait :: FilePath -> [String] -> String -> Int -> IO ()
runProcessWithInputAndWait cmd args input timeout = do
    pid <- forkProcess $ do
       forkProcess $ do -- double fork it over to init
         createSession
         (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
         hPutStr pin input
         hFlush pin
         threadDelay timeout
         hClose pin
         --     output <- hGetContents pout
         --     when (output==output) $ return ()
         hClose pout
         hClose perr
         waitForProcess ph
         return ()
       exitWith ExitSuccess
       return ()
    getProcessStatus True False pid
    return ()


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
