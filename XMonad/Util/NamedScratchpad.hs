{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.NamedScratchpad
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Named scratchpads that support several arbitrary applications at the same time.
--
-----------------------------------------------------------------------------

module XMonad.Util.NamedScratchpad (
  -- * Usage
  -- $usage
  NamedScratchpad(..),
  nonFloating,
  defaultFloating,
  customFloating,
  NamedScratchpads,
  namedScratchpadAction,
  allNamedScratchpadAction,
  namedScratchpadManageHook,
  ) where

import XMonad
import XMonad.Hooks.ManageHelpers (doRectFloat, isInProperty)
import XMonad.Actions.Minimize
import XMonad.Actions.CopyWindow

import Control.Monad (filterM)
import Data.Maybe (listToMaybe)

import qualified XMonad.StackSet as W


-- $usage
-- Allows to have several floating scratchpads running different applications.
-- Bind a key to 'namedScratchpadSpawnAction'.
-- Pressing it will spawn configured application, or bring it to the current
-- workspace if it already exists.
-- Pressing the key with the application on the current workspace will
-- send it to a hidden workspace called @NSP@.
--
-- If you already have a workspace called @NSP@, it will use that.
-- @NSP@ will also appear in xmobar and dzen status bars. You can tweak your
-- @dynamicLog@ settings to filter it out if you like.
--
-- Create named scratchpads configuration in your xmonad.hs like this:
--
-- > import XMonad.StackSet as W
-- > import XMonad.ManageHook
-- > import XMonad.Util.NamedScratchpad
-- >
-- > scratchpads = [
-- > -- run htop in xterm, find it by title, use default floating window placement
-- >     NS "htop" "xterm -e htop" (title =? "htop") defaultFloating ,
-- >
-- > -- run stardict, find it by class name, place it in the floating window
-- > -- 1/6 of screen width from the left, 1/6 of screen height
-- > -- from the top, 2/3 of screen width by 2/3 of screen height
-- >     NS "stardict" "stardict" (className =? "Stardict")
-- >         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,
-- >
-- > -- run gvim, find by role, don't float
-- >     NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") nonFloating
-- > ] where role = stringProperty "WM_WINDOW_ROLE"
--
-- Add keybindings:
--
-- >  , ((modm .|. controlMask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "htop")
-- >  , ((modm .|. controlMask .|. shiftMask, xK_s), namedScratchpadAction scratchpads "stardict")
-- >  , ((modm .|. controlMask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "notes")
--
-- ... and a manage hook:
--
-- >  , manageHook = namedScratchpadManageHook scratchpads
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings"
--

-- | Single named scratchpad configuration
data NamedScratchpad = NS { name   :: String      -- ^ Scratchpad name
                          , cmd    :: String      -- ^ Command used to run application
                          , query  :: Query Bool  -- ^ Query to find already running application
                          , hook   :: ManageHook  -- ^ Manage hook called for application window, use it to define the placement. See @nonFloating@, @defaultFloating@ and @customFloating@
                          }

-- | Manage hook that makes the window non-floating
nonFloating :: ManageHook
nonFloating = idHook

-- | Manage hook that makes the window floating with the default placement
defaultFloating :: ManageHook
defaultFloating = doFloat

-- | Manage hook that makes the window floating with custom placement
customFloating :: W.RationalRect -> ManageHook
customFloating = doRectFloat

-- | Named scratchpads configuration
type NamedScratchpads = [NamedScratchpad]

-- | Finds named scratchpad configuration by name
findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = listToMaybe $ filter ((s==) . name) c

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Action to pop up specified named scratchpad
namedScratchpadAction :: NamedScratchpads -- ^ Named scratchpads configuration
                      -> String           -- ^ Scratchpad name
                      -> X ()
namedScratchpadAction = someNamedScratchpadAction (\f ws -> f $ head ws)

allNamedScratchpadAction :: NamedScratchpads
                         -> String
                         -> X ()
allNamedScratchpadAction = someNamedScratchpadAction mapM_

someNamedScratchpadAction :: ((Window -> X ()) -> [Window] -> X ())
                          -> NamedScratchpads
                          -> String
                          -> X ()
someNamedScratchpadAction f confs n
    | Just conf <- findByName confs n = withWindowSet $ \s -> do
        allScratchpads <- filterM (runQuery (query conf)) (W.allWindows s)
        let allWindows = (maybe [] W.integrate . W.stack . W.workspace . W.current) s
        scratchpads <- filterM (runQuery (query conf)) allWindows
        case allScratchpads of
          [] -> runApplication conf
          scratchpad : _ -> case scratchpads of
            [] -> windows $ copyWindowToAll scratchpad
            _ ->  f toggleMinimize scratchpads
    | otherwise = return ()
  where
    toggleMinimize :: Window -> X ()
    toggleMinimize window = runQuery isMinimized window >>= \case
        True -> maximizeWindow window >> windows (W.focusWindow window)
        False -> minimizeWindow window

    isMinimized :: Query Bool
    isMinimized = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

    copyWindowToAll :: (Eq s, Eq i, Eq a) => a -> W.StackSet i l a s sd
                                               -> W.StackSet i l a s sd
    copyWindowToAll w s = foldr (copyWindow w) s $ map W.tag (W.workspaces s)


-- | Manage hook to use with named scratchpads
namedScratchpadManageHook :: NamedScratchpads -- ^ Named scratchpads configuration
                          -> ManageHook
namedScratchpadManageHook = composeAll . fmap (\c -> query c --> (hook c >> doF copyToAll))

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
