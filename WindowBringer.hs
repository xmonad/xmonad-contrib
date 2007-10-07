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
                                    gotoMenu, bringMenu
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
-- WindowBringer brings windows to you and you to windows.
-- That is to say, it pops up a dmenu with window names, in case you forgot
-- where you left your XChat.
--
-- Place in your Config.hs:
-- > import XMonadContrib.WindowBringer
-- and in the keys definition:
-- > , ((modMask .|. shiftMask, xK_g     ), gotoMenu)
-- > , ((modMask .|. shiftMask, xK_b     ), bringMenu)
--
-- %import XMonadContrib.WindowBringer
-- %keybind ((modMask .|. shiftMask, xK_g     ), gotoMenu)
-- %keybind ((modMask .|. shiftMask, xK_b     ), bringMenu)

-- | Pops open a dmenu with window titles. Choose one, and you will be
-- taken to the corresponding workspace.
gotoMenu :: X ()
gotoMenu = workspaceMap >>= actionMenu (windows . W.greedyView)
 where workspaceMap = windowMapWith (W.tag . fst)

-- | Pops open a dmenu with window titles. Choose one, and it will be
-- dragged, kicking and screaming, into your current workspace.
bringMenu :: X ()
bringMenu = windowMap >>= actionMenu (windows . bringWindow)
 where windowMap = windowMapWith snd
       bringWindow w ws = W.shiftWin (W.tag . W.workspace . W.current $ ws) w ws

-- | Calls dmenuMap to grab the appropriate element from the Map, and hands it
-- off to action if found.
actionMenu :: (a -> X ()) -> M.Map String a -> X ()
actionMenu action windowMap = dmenuMap windowMap >>= flip X.whenJust action

-- | Generates a Map from window name to <whatever you specify>. For use with
-- dmenuMap. TODO: extract the pure, creamy center.
windowMapWith :: ((X.WindowSpace, Window) -> a) -> X (M.Map String a)
windowMapWith value = do
  ws <- gets X.windowset
  M.fromList `fmap` concat `fmap` mapM keyValuePairs (W.workspaces ws)
 where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
       keyValuePair ws w = flip (,) (value (ws, w)) `fmap` decorateName ws w

-- | Returns the window name as will be listed in dmenu.
-- Lowercased, for your convenience (since dmenu is case-sensitive).
-- Tagged with the workspace ID, to guarantee uniqueness, and to let the user know where he's going.
decorateName :: X.WindowSpace -> Window -> X String
decorateName ws w = do
  name <- fmap (map toLower . show) $ getName w
  return $ name ++ " [" ++ W.tag ws ++ "]"
