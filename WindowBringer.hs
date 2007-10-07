-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.WindowBringer
-- Copyright   :  Devin Mullins <me@twifkak.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- dmenu operations to bring windows to you, and bring you to windows.
--
-----------------------------------------------------------------------------

module XMonadContrib.WindowBringer (
                            -- * Usage
                            -- $usage 
                            gotoMenu
                           ) where

import Control.Monad.State (gets)
import Data.Char (toLower)
import qualified Data.Map as M
import Graphics.X11.Xlib (Window())

import Operations (windows)
import qualified StackSet as W
import XMonad (X)
import qualified XMonad as X
import XMonadContrib.Dmenu (dmenuMap)
import XMonadContrib.NamedWindows (getName)

-- $usage
-- WindowBringer brings you to windows. (A future edition will bring windows to
-- you.)
--
-- Place in your Config.hs:
-- > import XMonadContrib.WindowBringer
-- and in the keys definition:
-- > , ((modMask .|. shiftMask, xK_g     ), gotoMenu)
--
-- %import XMonadContrib.WindowBringer
-- %keybind ((modMask .|. shiftMask, xK_g     ), gotoMenu)

-- | Pops open a dmenu with window titles. Choose one, and you will be
-- taken to the corresponding workspace.
gotoMenu :: X ()
gotoMenu = do
  workspaceMap >>= dmenuMap >>= flip X.whenJust (windows . W.greedyView)

-- | A map from decorated window name to target workspace ID, for use by gotoMenu.
workspaceMap :: X (M.Map String X.WorkspaceId)
workspaceMap = do
  ws <- gets X.windowset
  M.fromList `fmap` concat `fmap` mapM keyValuePairs (W.workspaces ws)
 where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
       keyValuePair ws w = flip (,) (W.tag ws) `fmap` decorateName ws w

-- | Returns the window name as will be listed in dmenu.
-- Lowercased, for your convenience (since dmenu is case-sensitive).
-- Tagged with the workspace ID, to guarantee uniqueness, and to let the user know where he's going.
decorateName :: X.WindowSpace -> Window -> X String
decorateName ws w = do
  name <- fmap (map toLower . show) $ getName w
  return $ name ++ " [" ++ W.tag ws ++ "]"
