{-# LANGUAGE TupleSections #-}
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
                    WindowBringerConfig(..),
                    gotoMenu, gotoMenuConfig, gotoMenu', gotoMenuArgs, gotoMenuArgs',
                    bringMenu, bringMenuConfig, bringMenu', bringMenuArgs, bringMenuArgs',
                    windowMap, windowAppMap, windowMap', bringWindow, actionMenu
                   ) where

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import XMonad
import qualified XMonad as X
import XMonad.Util.Dmenu (menuMapArgs)
import XMonad.Util.NamedWindows (getName, getNameWMClass)

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

data WindowBringerConfig = WindowBringerConfig
    { menuCommand :: String -- ^ The shell command that will handle window selection
    , menuArgs :: [String] -- ^ Arguments to be passed to menuCommand
    , windowTitler :: X.WindowSpace -> Window -> X String -- ^ A function that produces window titles given a workspace and a window
    }

instance Default WindowBringerConfig where
    def = WindowBringerConfig{ menuCommand = "dmenu"
                             , menuArgs = ["-i"]
                             , windowTitler = decorateName
                             }

-- | Pops open a dmenu with window titles. Choose one, and you will be
--   taken to the corresponding workspace.
gotoMenu :: X ()
gotoMenu = gotoMenuConfig def

-- | Pops open a dmenu with window titles. Choose one, and you will be
--   taken to the corresponding workspace. This version accepts a configuration
--   object.
gotoMenuConfig :: WindowBringerConfig -> X ()
gotoMenuConfig wbConfig = actionMenu wbConfig W.focusWindow

-- | Pops open a dmenu with window titles. Choose one, and you will be
--   taken to the corresponding workspace. This version takes a list of
--   arguments to pass to dmenu.
gotoMenuArgs :: [String] -> X ()
gotoMenuArgs args = gotoMenuConfig def { menuArgs = args }

-- | Pops open an application with window titles given over stdin. Choose one,
--   and you will be taken to the corresponding workspace.
gotoMenu' :: String -> X ()
gotoMenu' cmd = gotoMenuConfig def { menuArgs = [], menuCommand = cmd }

-- | Pops open an application with window titles given over stdin. Choose one,
--   and you will be taken to the corresponding workspace. This version takes a
--   list of arguments to pass to dmenu.
gotoMenuArgs' :: String -> [String] -> X ()
gotoMenuArgs' cmd args = gotoMenuConfig def { menuCommand = cmd, menuArgs = args }

-- | Pops open a dmenu with window titles. Choose one, and it will be
--   dragged, kicking and screaming, into your current workspace.
bringMenu :: X ()
bringMenu = bringMenuArgs def

-- | Pops open a dmenu with window titles. Choose one, and it will be
--   dragged, kicking and screaming, into your current workspace. This version
--   accepts a configuration object.
bringMenuConfig :: WindowBringerConfig -> X ()
bringMenuConfig wbConfig = actionMenu wbConfig bringWindow

-- | Pops open a dmenu with window titles. Choose one, and it will be
--   dragged, kicking and screaming, into your current workspace. This version
--   takes a list of arguments to pass to dmenu.
bringMenuArgs :: [String] -> X ()
bringMenuArgs args = bringMenuConfig def { menuArgs = args }

-- | Pops open an application with window titles given over stdin. Choose one,
--   and it will be dragged, kicking and screaming, into your current
--   workspace.
bringMenu' :: String -> X ()
bringMenu' cmd = bringMenuConfig def { menuArgs = [], menuCommand = cmd }

-- | Pops open an application with window titles given over stdin. Choose one,
--   and it will be dragged, kicking and screaming, into your current
--   workspace. This version allows arguments to the chooser to be specified.
bringMenuArgs' :: String -> [String] -> X ()
bringMenuArgs' cmd args = bringMenuConfig def { menuArgs = args, menuCommand = cmd }

-- | Brings the specified window into the current workspace.
bringWindow :: Window -> X.WindowSet -> X.WindowSet
bringWindow w ws = W.shiftWin (W.currentTag ws) w ws

-- | Calls dmenuMap to grab the appropriate Window, and hands it off to action
--   if found.
actionMenu :: WindowBringerConfig -> (Window -> X.WindowSet -> X.WindowSet) -> X ()
actionMenu WindowBringerConfig{ menuCommand = cmd
                              , menuArgs = args
                              , windowTitler = titler
                              } action
    = windowMap' titler >>= menuMapFunction >>= flip X.whenJust (windows . action)
    where
      menuMapFunction :: M.Map String a -> X (Maybe a)
      menuMapFunction = menuMapArgs cmd args


-- | A map from window names to Windows.
windowMap :: X (M.Map String Window)
windowMap = windowMap' decorateName

-- | A map from application executable names to Windows.
windowAppMap :: X (M.Map String Window)
windowAppMap = windowMap' decorateAppName

-- | A map from window names to Windows, given a windowTitler function.
windowMap' :: (X.WindowSpace -> Window -> X String) -> X (M.Map String Window)
windowMap' titler = do
  ws <- gets X.windowset
  M.fromList . concat <$> mapM keyValuePairs (W.workspaces ws)
 where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
       keyValuePair ws w = flip (,) w <$> titler ws w

-- | Returns the window name as will be listed in dmenu.
--   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
--   know where he's going.
decorateName :: X.WindowSpace -> Window -> X String
decorateName ws w = do
  name <- show <$> getName w
  return $ name ++ " [" ++ W.tag ws ++ "]"

-- | Returns the window name as will be listed in dmenu.  This will
-- return the executable name of the window along with it's workspace
-- ID.
decorateAppName :: X.WindowSpace -> Window -> X String
decorateAppName ws w = do
  name <- show <$> getNameWMClass w
  return $ name ++ " [" ++ W.tag ws ++ "]"
