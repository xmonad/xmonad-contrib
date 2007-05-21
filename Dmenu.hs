module XMonadContrib.Dmenu (dmenu, dmenuXinerama, runProcessWithInput) where

import XMonad
import qualified StackSet as W
import System.Process
import System.IO
import Control.Monad.State

runProcessWithInput :: FilePath -> [String] -> String -> IO String
runProcessWithInput cmd args input = do
    (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output==output) $ return ()
    hClose pout
    hClose perr
    waitForProcess ph
    return output
    
-- Starts dmenu on the current screen. Requires this patch to dmenu:
-- http://www.jcreigh.com/dmenu/dmenu-2.8-xinerama.patch
dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `liftM` gets windowset :: X Int
    io $ runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)

dmenu :: [String] -> X String
dmenu opts = io $ runProcessWithInput "dmenu" [] (unlines opts)

