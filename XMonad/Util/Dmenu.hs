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
                dmenu, dmenuXinerama, dmenuMap, menu, menuArgs, menuMap, menuMapArgs
               ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.Run

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonad.Util.Dmenu
--
-- These functions block xmonad's event loop until dmenu exits; this means that
-- programs will not be able to open new windows and you will not be able to
-- change workspaces or input focus until you have responded to the prompt one
-- way or another.

-- %import XMonad.Util.Dmenu

-- | Starts dmenu on the current screen. Requires this patch to dmenu:
-- <http://www.jcreigh.com/dmenu/dmenu-3.2-xinerama.patch>
dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    curscreen <- (fromIntegral . W.screen . W.current) `fmap` gets windowset :: X Int
    runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)
    menuArgs "dmenu" ["-xs", show (curscreen+1)] opts

-- | Run dmenu to select an option from a list.
dmenu :: [String] -> X String
dmenu opts = menu "dmenu" opts

-- | like 'dmenu' but also takes the command to run.
menu :: String -> [String] -> X String
menu menuCmd opts = menuArgs menuCmd [] opts

-- | Like 'menu' but also takes a list of command line arguments.
menuArgs :: String -> [String] -> [String] -> X String
menuArgs menuCmd args opts = runProcessWithInput menuCmd args (unlines opts)

-- | Like 'dmenuMap' but also takes the command to run.
menuMap :: String -> M.Map String a -> X (Maybe a)
menuMap menuCmd selectionMap = menuMapArgs menuCmd [] selectionMap

-- | Like 'menuMap' but also takes a list of command line arguments.
menuMapArgs :: String -> [String] -> M.Map String a -> X (Maybe a)
menuMapArgs menuCmd args selectionMap = do
  selection <- menuFunction (M.keys selectionMap)
  return $ M.lookup selection selectionMap
      where
        menuFunction = menuArgs menuCmd args

-- | Run dmenu to select an entry from a map based on the key.
dmenuMap :: M.Map String a -> X (Maybe a)
dmenuMap selectionMap = menuMap "dmenu" selectionMap
