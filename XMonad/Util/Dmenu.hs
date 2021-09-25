-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Dmenu
-- Description :  Convenient bindings to dmenu.
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
    curscreen <-
      fromIntegral . W.screen . W.current <$> gets windowset :: X Int
    _ <-
      runProcessWithInput "dmenu" ["-xs", show (curscreen+1)] (unlines opts)
    menuArgs "dmenu" ["-xs", show (curscreen+1)] opts

-- | Run dmenu to select an option from a list.
dmenu :: MonadIO m => [String] -> m String
dmenu = menu "dmenu"

-- | like 'dmenu' but also takes the command to run.
menu :: MonadIO m => String -> [String] -> m String
menu menuCmd = menuArgs menuCmd []

-- | Like 'menu' but also takes a list of command line arguments.
menuArgs :: MonadIO m => String -> [String] -> [String] -> m String
menuArgs menuCmd args opts = filter (/='\n') <$>
  runProcessWithInput menuCmd args (unlines opts)

-- | Like 'dmenuMap' but also takes the command to run.
menuMap :: MonadIO m => String -> M.Map String a -> m (Maybe a)
menuMap menuCmd = menuMapArgs menuCmd []

-- | Like 'menuMap' but also takes a list of command line arguments.
menuMapArgs :: MonadIO m => String -> [String] -> M.Map String a ->
               m (Maybe a)
menuMapArgs menuCmd args selectionMap = do
  selection <- menuFunction (M.keys selectionMap)
  return $ M.lookup selection selectionMap
      where
        menuFunction = menuArgs menuCmd args

-- | Run dmenu to select an entry from a map based on the key.
dmenuMap :: MonadIO m => M.Map String a -> m (Maybe a)
dmenuMap = menuMap "dmenu"
