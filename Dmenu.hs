-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Dmenu
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A convenient binding to dmenu.
--
-----------------------------------------------------------------------------

module XMonadContrib.Dmenu (
                            -- * Usage
                            -- $usage 
                            dmenu, dmenuXinerama, 
                            runProcessWithInput
                           ) where

import XMonad
import qualified StackSet as W
import System.Exit
import System.Process
import System.IO
import Control.Monad.State

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.Dmenu

-- %import XMonadContrib.Dmenu

-- | Returns Just output if the command succeeded, and Nothing if it didn't.
-- This corresponds to dmenu's notion of exit code 1 for a cancelled invocation.
runProcessWithInput :: FilePath -> [String] -> String -> IO (Maybe String)
runProcessWithInput cmd args input = do
    (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output==output) $ return ()
    hClose pout
    hClose perr
    exitCode <- waitForProcess ph
    case exitCode of
      ExitSuccess   -> return (Just output)
      ExitFailure _ -> return Nothing

-- | Starts dmenu on the current screen. Requires this patch to dmenu:
-- <http://www.jcreigh.com/dmenu/dmenu-3.2-xinerama.patch>
dmenuXinerama :: [String] -> X (Maybe String)
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `liftM` gets windowset :: X Int
    io $ runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)

dmenu :: [String] -> X (Maybe String)
dmenu opts = io $ runProcessWithInput "dmenu" [] (unlines opts)
