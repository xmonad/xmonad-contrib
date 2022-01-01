----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Minimize
-- Description :  Actions for minimizing and maximizing windows.
-- Copyright   :  (c) Bogdan Sinitsyn (2016)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Bogdan Sinitsyn <bogdan.sinitsyn@gmail.com>
-- Stability   :  unstable
-- Portability :  not portable
--
-- Adds actions for minimizing and maximizing windows
--
-- This module should be used with "XMonad.Layout.Minimize". Add 'minimize' to your
-- layout modifiers as described in "XMonad.Layout.Minimized" and use actions from
-- this module
--
-- Possible keybindings:
--
-- >        , ((modm,               xK_m     ), withFocused minimizeWindow)
-- >        , ((modm .|. shiftMask, xK_m     ), withLastMinimized maximizeWindowAndFocus)
--
-----------------------------------------------------------------------------

module XMonad.Actions.Minimize
  ( -- * Usage
    -- $usage
    minimizeWindow
  , maximizeWindow
  , maximizeWindowAndFocus
  , withLastMinimized
  , withLastMinimized'
  , withFirstMinimized
  , withFirstMinimized'
  , withMinimized
  ) where

import XMonad
import XMonad.Prelude (fromMaybe, join, listToMaybe)
import qualified XMonad.StackSet as W

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Minimize
import XMonad.Util.WindowProperties (getProp32)

import Foreign.C.Types (CLong)
import qualified Data.List as L
import qualified Data.Map as M

-- $usage
-- Import this module with "XMonad.Layout.Minimize" and "XMonad.Layout.BoringWindows":
--
-- > import XMonad.Actions.Minimize
-- > import XMonad.Layout.Minimize
-- > import qualified XMonad.Layout.BoringWindows as BW
--
-- Then apply 'minimize' and 'boringWindows' to your layout hook and use some
-- actions from this module:
--
-- > main = xmonad def { layoutHook = minimize . BW.boringWindows $ whatever }
--
-- Example keybindings:
--
-- >        , ((modm,               xK_m     ), withFocused minimizeWindow      )
-- >        , ((modm .|. shiftMask, xK_m     ), withLastMinimized maximizeWindow)

setMinimizedState :: Window -> Int -> (CLong -> [CLong] -> [CLong]) -> X ()
setMinimizedState win st f = do
    setWMState win st
    withDisplay $ \dpy -> do
        wm_state <- getAtom "_NET_WM_STATE"
        hidden <- fromIntegral <$> getAtom "_NET_WM_STATE_HIDDEN"
        wstate <- fromMaybe [] <$> getProp32 wm_state win
        io $ changeProperty32 dpy win wm_state aTOM propModeReplace (f hidden wstate)

setMinimized :: Window -> X ()
setMinimized win = setMinimizedState win iconicState (:)

setNotMinimized :: Window -> X ()
setNotMinimized win = setMinimizedState win normalState L.delete

-- It does not just set minimizedStack to newWindows because it should save
-- order in which elements were added (newer first)
modified :: (RectMap -> RectMap) -> X Bool
modified f = XS.modified $
    \Minimized { rectMap = oldRectMap, minimizedStack = oldStack } ->
      let newRectMap = f oldRectMap
          newWindows = M.keys newRectMap
       in Minimized { rectMap = newRectMap
                    , minimizedStack = (newWindows L.\\ oldStack)
                                       ++
                                       (oldStack `L.intersect` newWindows)
                    }


-- | Minimize a window
minimizeWindow :: Window -> X ()
minimizeWindow w = withWindowSet $ \ws ->
  whenX (modified $ M.insert w (M.lookup w $ W.floating ws)) $ do
    setMinimized w
    windows $ W.sink w
    BW.focusDown


-- | Maximize window and apply a function to maximized window and 'WindowSet'
maximizeWindowAndChangeWSet :: (Window -> WindowSet -> WindowSet) -> Window -> X ()
maximizeWindowAndChangeWSet f w = do
  mrect <- XS.gets (join . M.lookup w . rectMap)
  whenX (modified $ M.delete w) $ do
    setNotMinimized w
    broadcastMessage BW.UpdateBoring
    windows $ f w . maybe id (W.float w) mrect

-- | Just maximize a window without focusing
maximizeWindow :: Window -> X ()
maximizeWindow = maximizeWindowAndChangeWSet $ const id

-- | Maximize a window and then focus it
maximizeWindowAndFocus :: Window -> X ()
maximizeWindowAndFocus = maximizeWindowAndChangeWSet W.focusWindow

-- | Perform an action with first minimized window on current workspace
--   or do nothing if there is no minimized windows on current workspace
withFirstMinimized :: (Window -> X ()) -> X ()
withFirstMinimized action = withFirstMinimized' (`whenJust` action)

-- | Like withFirstMinimized but the provided action is always invoked with a
--   'Maybe Window', that will be nothing if there is no first minimized window.
withFirstMinimized' :: (Maybe Window -> X ()) -> X ()
withFirstMinimized' action = withMinimized (action . listToMaybe . reverse)

-- | Perform an action with last minimized window on current workspace
--   or do nothing if there is no minimized windows on current workspace
withLastMinimized :: (Window -> X ()) -> X ()
withLastMinimized action = withLastMinimized' (`whenJust` action)

-- | Like withLastMinimized but the provided action is always invoked with a
--   'Maybe Window', that will be nothing if there is no last minimized window.
withLastMinimized' :: (Maybe Window -> X ()) -> X ()
withLastMinimized' action = withMinimized (action . listToMaybe)

withMinimized :: ([Window] -> X a) -> X a
withMinimized action = do
  minimized <- XS.gets minimizedStack
  currentStack <- withWindowSet $ return . W.index
  action $ minimized `L.intersect` currentStack
