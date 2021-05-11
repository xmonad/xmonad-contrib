{-# LANGUAGE MultiWayIf #-}
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
                                cycleRecentNonEmptyWS,
                                cycleWindowSets,
                                toggleRecentWS,
                                toggleRecentNonEmptyWS,
                                toggleWindowSets,
                                recentWS
) where

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter)

import Control.Arrow ((&&&))
import Data.List (find)
import Data.Maybe (fromJust)

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
cycleRecentWS = cycleWindowSets $ recentWS (const True)


-- | Like 'cycleRecentWS', but restricted to non-empty workspaces.
cycleRecentNonEmptyWS :: [KeySym] -- ^ A list of modifier keys used when invoking this action.
                                  --   As soon as one of them is released, the final switch is made.
                      -> KeySym   -- ^ Key used to switch to next (less recent) workspace.
                      -> KeySym   -- ^ Key used to switch to previous (more recent) workspace.
                                  --   If it's the same as the nextWorkspace key, it is effectively ignored.
                      -> X ()
cycleRecentNonEmptyWS = cycleWindowSets $ recentWS (not . null . stack)


-- | Switch to the most recent workspace. The stack of most recently used workspaces
-- is updated, so repeated use toggles between a pair of workspaces.
toggleRecentWS :: X ()
toggleRecentWS = toggleWindowSets $ recentWS (const True)


-- | Like 'toggleRecentWS', but restricted to non-empty workspaces.
toggleRecentNonEmptyWS :: X ()
toggleRecentNonEmptyWS = toggleWindowSets $ recentWS (not . null . stack)


-- | Given a predicate @p@ and the current 'WindowSet' @w@, create a
-- list of workspaces to choose from. They are ordered by recency and
-- have to satisfy @p@.
recentWS :: (WindowSpace -> Bool) -- ^ A workspace predicate.
         -> WindowSet             -- ^ The current WindowSet
         -> [WorkspaceId]
recentWS p w = map tag
             $ filter p
             $ map workspace (visible w)
               ++ hidden w
               ++ [workspace (current w)]

-- | Cycle through a finite list of workspaces with repeated presses of a key, while
--   a modifier key is held down. For best effects use the same modkey+key combination
--   as the one used to invoke this action.
cycleWindowSets :: (WindowSet -> [WorkspaceId]) -- ^ A function used to create a list of workspaces to choose from
                -> [KeySym]                     -- ^ A list of modifier keys used when invoking this action.
                                                --   As soon as one of them is released, the final workspace is chosen and the action exits.
                -> KeySym                       -- ^ Key used to preview next workspace from the list of generated options
                -> KeySym                       -- ^ Key used to preview previous workspace from the list of generated options.
                                                --   If it's the same as nextOption key, it is effectively ignored.
                -> X ()
cycleWindowSets genOptions mods keyNext keyPrev = do
  origWSet <- gets windowset
  let options = genOptions origWSet
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
                maskEvent d (keyPressMask .|. keyReleaseMask) p
                KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                s <- keycodeToKeysym d c 0
                return (t, s)
  let setOption n = do
        let nextWs   = options `cycref` n
            syncW ws = windows $ view ws . restoreOrder origWSet
        (t, s) <- io event
        if | t == keyPress   && s == keyNext  -> syncW nextWs >> setOption (n + 1)
           | t == keyPress   && s == keyPrev  -> syncW nextWs >> setOption (n - 1)
           | t == keyRelease && s `elem` mods ->
               syncW =<< gets (tag . workspace . current . windowset)
           | otherwise                        -> setOption n
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  windows $ view (options `cycref` 0) -- view the first ws
  setOption 1
  io $ ungrabKeyboard d currentTime
 where
  cycref :: [a] -> Int -> a
  cycref l i = l !! (i `mod` length l)

  -- | Given an old and a new 'WindowSet', which is __exactly__ one
  -- 'view' away from the old one, restore the workspace order of the
  -- former inside of the latter.  This respects any new state that the
  -- new 'WindowSet' may have accumulated.
  restoreOrder :: WindowSet -> WindowSet -> WindowSet
  restoreOrder origW w
    | -- The focused screen changed; the old focus is on a visible screen
      newFocT `elem` visT =
        w { current = (current origW){ workspace = findFoc (workspace <$> visible w) }
          , visible = zipWith (\v ws -> v{ workspace = ws }) vis (updateNewFoc visW)
          }
    | -- The focused screen didn't change; the old focus is hidden
      newFocT `elem` hidT =
        w { current = (current origW){ workspace = findFoc (hidden w) }
          , hidden  = updateNewFoc hid
          }
    | otherwise = w
   where
    -- Foci, old and new
    focT              = tag . workspace . current $ origW
    (newFoc, newFocT) = id &&& tag $ workspace (current w)

    -- Workspaces in the _original_ windowset
    (hid, hidT)         = id &&& map tag $ hidden origW
    (vis, (visW, visT)) = id &&& map workspace &&& map (tag . workspace)
                        $ visible origW

    -- | Given a list of new workspaces that contain the old focus,
    -- return that workspace.
    findFoc :: [WindowSpace] -> WindowSpace
    findFoc ws = fromJust $ find ((== focT) . tag) ws

    -- | Given a list of old workspaces that contain the new focus,
    -- update the state of the focus.
    updateNewFoc :: [WindowSpace] -> [WindowSpace]
    updateNewFoc ws = before ++ newFoc : after
     where (before, after) = drop 1 <$> break ((== newFocT) . tag) ws

-- | Given some function that generates a list of workspaces from a
-- given 'WindowSet', switch to the first generated workspace.
toggleWindowSets :: (WindowSet -> [WorkspaceId]) -> X ()
toggleWindowSets genOptions = do
  options <- gets $ genOptions . windowset
  case options of
    []  -> return ()
    o:_ -> windows (view o)
