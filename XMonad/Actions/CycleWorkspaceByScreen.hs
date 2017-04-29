
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleWorkspaceByScreen
-- Copyright   :  (c) 2017 Ivan Malison
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  IvanMalison@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Cycle through previously viewed workspaces in the order they were viewed most
-- recently on the screen where cycling is taking place.
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleWorkspaceByScreen (
    -- * Usage
    -- $usage
    cycleWorkspaceOnScreen
  , cycleWorkspaceOnCurrentScreen
  , handleKeyEvent
  , repeatableAction
  ) where

import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Maybe

import           Graphics.X11.Xlib.Extras

import           XMonad
import           XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W

-- $usage
-- This module must be used in conjuction with XMonad.Hooks.WorkspaceHistory
--
-- To use, add something like the following to your keybindings
-- , ((mod4Mask,  xK_slash), cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_slash xK_p)

repeatableAction :: [KeySym] -> (EventType -> KeySym -> X ()) -> X ()
repeatableAction mods pressHandler = do
  XConf {theRoot = root, display = d} <- ask
  let getNextEvent = io $ allocaXEvent $ \p ->
                 do
                   maskEvent d (keyPressMask .|. keyReleaseMask) p
                   KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                   s <- io $ keycodeToKeysym d c 0
                   return (t, s)
      handleEvent (t, s)
        | t == keyRelease && s `elem` mods = return ()
        | otherwise = (pressHandler t s) >> getNextEvent >>= handleEvent

  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  getNextEvent >>= handleEvent
  io $ ungrabKeyboard d currentTime

handleKeyEvent :: EventType
               -> KeySym
               -> X ()
               -> EventType
               -> KeySym
               -> Maybe (X ())
handleKeyEvent eventType key action = helper
  where
  helper et k
    | et == eventType && k == key = Just action
    | otherwise = Nothing


runFirst :: [EventType -> KeySym -> Maybe (X ())] -> EventType -> KeySym -> X ()
runFirst matchers eventType key =
  fromMaybe (return ()) $ join $ find isJust $ map (\fn -> fn eventType key) matchers

cycleWorkspaceOnScreen :: ScreenId -> [KeySym] -> KeySym -> KeySym -> X ()
cycleWorkspaceOnScreen screenId mods nextKey prevKey = workspaceHistoryTransaction $ do
  startingHistory <- workspaceHistoryByScreen
  currentWSIndex <- io $ newIORef 1
  let cycleWorkspaces = fromMaybe [] $ lookup screenId startingHistory
      getAndIncrementWS increment = do
        current <- readIORef currentWSIndex
        modifyIORef
          currentWSIndex
          ((`mod` (length cycleWorkspaces)) . (+ increment))
        return $ cycleWorkspaces !! current
      focusIncrement i = (io $ getAndIncrementWS i) >>= (windows . W.greedyView)

  focusIncrement 1 -- Do the first workspace cycle
  repeatableAction mods $
    runFirst
      [ handleKeyEvent keyPress nextKey $ focusIncrement 1
      , handleKeyEvent keyPress prevKey $ focusIncrement (-1)
      ]
  return ()

cycleWorkspaceOnCurrentScreen
  :: [KeySym] -> KeySym -> KeySym -> X ()
cycleWorkspaceOnCurrentScreen mods n p =
  withWindowSet $ \ws ->
    cycleWorkspaceOnScreen (W.screen $ W.current ws) mods n p
