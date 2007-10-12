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
-- Requires the process-1.0 package
--
-----------------------------------------------------------------------------

module XMonadContrib.Dmenu (
                            -- * Usage
                            -- $usage
                            dmenu, dmenuXinerama, dmenuMap
                           ) where

import XMonad
import qualified StackSet as W
import qualified Data.Map as M
import Control.Monad.State
import XMonadContrib.Run

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.Dmenu

-- %import XMonadContrib.Dmenu

-- | Starts dmenu on the current screen. Requires this patch to dmenu:
-- <http://www.jcreigh.com/dmenu/dmenu-3.2-xinerama.patch>
dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `liftM` gets windowset :: X Int
    io $ runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)

dmenu :: [String] -> X String
dmenu opts = io $ runProcessWithInput "dmenu" [] (unlines opts)

dmenuMap :: M.Map String a -> X (Maybe a)
dmenuMap selectionMap = do
  selection <- dmenu (M.keys selectionMap)
  return $ M.lookup selection selectionMap
