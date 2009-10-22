-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleRecentWS
-- Copyright   :  (c) Michal Janeczek <janeczek@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Michal Janeczek <janeczek@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle through most recently used workspaces
-- with repeated presses of a single key (as long as modifier key is
-- held down). This is similar to how many window managers handle
-- window switching.
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleRecentWS (
                                -- * Usage
                                -- $usage
                                cycleRecentWS,
                                cycleWindowSets
) where

import XMonad hiding (workspaces)
import XMonad.StackSet

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.CycleRecentWS
-- >
-- >   , ((modm, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Cycle through most recent workspaces with repeated presses of a key, while
--   a modifier key is held down. The recency of workspaces previewed while browsing
--   to the target workspace is not affected. That way a stack of most recently used
--   workspaces is maintained, similarly to how many window managers handle window
--   switching. For best effects use the same modkey+key combination as the one used
--   to invoke this action.
cycleRecentWS :: [KeySym] -- ^ A list of modifier keys used when invoking this action.
                          --   As soon as one of them is released, the final switch is made.
              -> KeySym   -- ^ Key used to switch to next (less recent) workspace.
              -> KeySym   -- ^ Key used to switch to previous (more recent) workspace.
                          --   If it's the same as the nextWorkspace key, it is effectively ignored.
              -> X ()
cycleRecentWS = cycleWindowSets options
 where options w = map (view `flip` w) (recentTags w)
       recentTags w = map tag $ tail (workspaces w) ++ [head (workspaces w)]


cycref :: [a] -> Int -> a
cycref l i = l !! (i `mod` length l)

-- | Cycle through a finite list of WindowSets with repeated presses of a key, while
--   a modifier key is held down. For best effects use the same modkey+key combination
--   as the one used to invoke this action.
cycleWindowSets :: (WindowSet -> [WindowSet]) -- ^ A function used to create a list of WindowSets to choose from
                -> [KeySym]                   -- ^ A list of modifier keys used when invoking this action.
                                              --   As soon as one of them is released, the final WindowSet is chosen and the action exits.
                -> KeySym                     -- ^ Key used to preview next WindowSet from the list of generated options
                -> KeySym                     -- ^ Key used to preview previous WindowSet from the list of generated options.
                                              --   If it's the same as nextOption key, it is effectively ignored.
                -> X ()
cycleWindowSets genOptions mods keyNext keyPrev = do
  options <- gets $ genOptions . windowset
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
                maskEvent d (keyPressMask .|. keyReleaseMask) p
                KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                s <- keycodeToKeysym d c 0
                return (t, s)
  let setOption n = do windows $ const $ options `cycref` n
                       (t, s) <- io event
                       case () of
                         () | t == keyPress   && s == keyNext  -> setOption (n+1)
                            | t == keyPress   && s == keyPrev  -> setOption (n-1)
                            | t == keyRelease && s `elem` mods -> return ()
                            | otherwise                        -> setOption n
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  setOption 0
  io $ ungrabKeyboard d currentTime
