-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Dmenu
-- Copyright   :  (c) Spencer Janssen <spencerjanssen@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A convenient binding to dmenu.
--
-- Requires the process-1.0 package
--
-----------------------------------------------------------------------------

module XMonad.Util.Dmenu (
                            -- * Usage
                            -- $usage
                            dmenu, dmenuXinerama, dmenuMap
                           ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.Run

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonad.Util.Dmenu

-- %import XMonad.Util.Dmenu

-- | Starts dmenu on the current screen. Requires this patch to dmenu:
-- <http://www.jcreigh.com/dmenu/dmenu-3.2-xinerama.patch>
dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `fmap` gets windowset :: X Int
    io $ runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)

dmenu :: [String] -> X String
dmenu opts = io $ runProcessWithInput "dmenu" [] (unlines opts)

dmenuMap :: M.Map String a -> X (Maybe a)
dmenuMap selectionMap = do
  selection <- dmenu (M.keys selectionMap)
  return $ M.lookup selection selectionMap
