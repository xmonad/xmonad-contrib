-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.WindowBringer
-- Copyright   :  Devin Mullins <me@twifkak.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- dmenu operations to bring windows to you, and bring you to windows.
-- That is to say, it pops up a dmenu with window names, in case you forgot
-- where you left your XChat.
--
-----------------------------------------------------------------------------

module XMonad.Actions.WindowBringer (
                    -- * Usage
                    -- $usage
                    gotoMenu, gotoMenu', gotoMenuArgs, gotoMenuArgs',
                    bringMenu, bringMenu', bringMenuArgs, bringMenuArgs',
                    windowMap,
                    bringWindow
                   ) where

import Data.Char (toLower)
import qualified Data.Map as M

import qualified XMonad.StackSet as W
import XMonad
import qualified XMonad as X
import XMonad.Util.Dmenu (menuMapArgs)
import XMonad.Util.NamedWindows (getName)

-- $usage
--
-- Import the module into your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.WindowBringer
--
-- and define appropriate key bindings:
--
-- > , ((modm .|. shiftMask, xK_g     ), gotoMenu)
-- > , ((modm .|. shiftMask, xK_b     ), bringMenu)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Default menu command
defaultCmd :: String
defaultCmd = "dmenu"

-- | Pops open a dmenu with window titles. Choose one, and you will be
--   taken to the corresponding workspace.
gotoMenu :: X ()
gotoMenu = gotoMenuArgs []

-- | Pops open a dmenu with window titles. Choose one, and you will be
--   taken to the corresponding workspace. This version takes a list of
--   arguments to pass to dmenu.
gotoMenuArgs :: [String] -> X ()
gotoMenuArgs menuArgs = gotoMenuArgs' defaultCmd menuArgs

-- | Pops open an application with window titles given over stdin. Choose one,
--   and you will be taken to the corresponding workspace.
gotoMenu' :: String -> X ()
gotoMenu' menuCmd = gotoMenuArgs' menuCmd []

-- | Pops open an application with window titles given over stdin. Choose one,
--   and you will be taken to the corresponding workspace. This version takes a
--   list of arguments to pass to dmenu.
gotoMenuArgs' :: String -> [String] -> X ()
gotoMenuArgs' menuCmd menuArgs = actionMenu menuCmd menuArgs W.focusWindow

-- | Pops open a dmenu with window titles. Choose one, and it will be
--   dragged, kicking and screaming, into your current workspace.
bringMenu :: X ()
bringMenu = bringMenuArgs []

-- | Pops open a dmenu with window titles. Choose one, and it will be
--   dragged, kicking and screaming, into your current workspace. This version
--   takes a list of arguments to pass to dmenu.
bringMenuArgs :: [String] -> X ()
bringMenuArgs menuArgs = bringMenuArgs' defaultCmd menuArgs

-- | Pops open an application with window titles given over stdin. Choose one,
--   and it will be dragged, kicking and screaming, into your current
--   workspace.
bringMenu' :: String -> X ()
bringMenu' menuCmd = bringMenuArgs' menuCmd []

-- | Pops open an application with window titles given over stdin. Choose one,
--   and it will be dragged, kicking and screaming, into your current
--   workspace. This version allows arguments to the chooser to be specified.
bringMenuArgs' :: String -> [String] -> X ()
bringMenuArgs' menuCmd menuArgs = actionMenu menuCmd menuArgs bringWindow

-- | Brings the specified window into the current workspace.
bringWindow :: Window -> X.WindowSet -> X.WindowSet
bringWindow w ws = W.shiftWin (W.currentTag ws) w ws

-- | Calls dmenuMap to grab the appropriate Window, and hands it off to action
--   if found.
actionMenu :: String -> [String] -> (Window -> X.WindowSet -> X.WindowSet) -> X ()
actionMenu menuCmd menuArgs action = windowMap >>= menuMapFunction >>= flip X.whenJust (windows . action)
    where
      menuMapFunction :: M.Map String a -> X (Maybe a)
      menuMapFunction selectionMap = menuMapArgs menuCmd menuArgs selectionMap

-- | A map from window names to Windows.
windowMap :: X (M.Map String Window)
windowMap = do
  ws <- gets X.windowset
  M.fromList `fmap` concat `fmap` mapM keyValuePairs (W.workspaces ws)
 where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
       keyValuePair ws w = flip (,) w `fmap` decorateName ws w

-- | Returns the window name as will be listed in dmenu.
--   Lowercased, for your convenience (since dmenu is case-sensitive).
--   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
--   know where he's going.
decorateName :: X.WindowSpace -> Window -> X String
decorateName ws w = do
  name <- fmap (map toLower . show) $ getName w
  return $ name ++ " [" ++ W.tag ws ++ "]"
