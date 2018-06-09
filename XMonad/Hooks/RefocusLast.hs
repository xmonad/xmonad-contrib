
--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.RefocusLast
-- Description :  Hooks and actions to refocus the previous window.
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides log and event hooks that keep track of recently focused windows on a
-- per workspace basis and automatically refocus the last window when the
-- current one is closed. Also provides an action to toggle focus between the
-- current and previous window, and one that refocuses appropriately on sending
-- the current window to another workspace.
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

module XMonad.Hooks.RefocusLast (
  -- * Usage
  -- $Usage
  RecentWins(..),
  RecentsMap(..),
  refocusLastLogHook,
  refocusLastEventHook,
  toggleFocus,
  shiftRL
) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import qualified Data.Map.Strict as M

-- }}}

-- --< Usage >-- {{{

-- $Usage
-- To use this module, you must include 'refocusLastLogHook' in your log hook.
-- This suffices to make use of both 'toggleFocus' and 'shiftRL' but will not
-- refocus automatically upon loss of the current window; for that you must also
-- include 'refocusLastEventHook' in your event hook.
--
-- Example configuration:
--
-- > import XMonad
-- > import XMonad.Hooks.RefocusLast
-- > import qualified Data.Map.Strict as M
-- >
-- > main = xmonad def
-- >   { handleEventHook = refocusLastEventHook <+> handleEventHook def
-- >   , logHook         = refocusLastLogHook   <+> logHook         def
-- >   , keys            = rlKeys               <+> keys            def
-- >   } where rlKeys = \cnf -> M.fromList
-- >                  $ ((modMask cnf, xK_a), toggleFocus)
-- >                  : [ ((modMask cnf .|. shiftMask, n), shiftRL wksp)
-- >                    | (n, wksp) <- zip [xK_1..xK_9] (workspaces cnf)
-- >                    ]

-- }}}

-- --< Types >-- {{{

-- | Data type holding onto the previous and current @Window@.
data RecentWins = Recent { previous :: !Window, current :: !Window }
  deriving (Show, Read, Eq, Typeable)

-- | Newtype wrapper for a @Map@ holding a @Recent@ for each workspace.
--   Is an instance of @ExtensionClass@ with persistence of state.
newtype RecentsMap = RecentsMap (M.Map WorkspaceId RecentWins)
  deriving (Show, Read, Eq, Typeable)

instance ExtensionClass RecentsMap where
  initialValue = RecentsMap M.empty
  extensionType = PersistentExtension

-- }}}

-- --< Public Hooks & Actions >-- {{{

-- | This log hook is what records recently focused windows into extensible
--   state.
refocusLastLogHook :: X ()
refocusLastLogHook = withFocii $ \fw tag -> do
  m <- getRecentsMap
  let insertRecent la ca = XS.put . RecentsMap $ M.insert tag (Recent la ca) m
  case M.lookup tag m of
    Just (Recent _ cw) -> when (cw /= fw) (insertRecent cw fw)
    Nothing            ->                  insertRecent fw fw

-- | This event hook runs iff the core xmonad event handler will unmanage the
--   event window, and shifts focus to the last focused window if possible.
refocusLastEventHook :: Event -> X All
refocusLastEventHook ev = All True <$ handle ev
  where
    handle (DestroyWindowEvent {                ev_window = w }) = refocusLast w
    handle (UnmapEvent { ev_send_event = synth, ev_window = w }) = do
      e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
      when (synth || e == 0) (refocusLast w)
    handle  _                                                    = return ()
    refocusLast w = withFocAndRec $ \_ _ (Recent lw cw) ->
      when (w == cw) (modify $ refocus lw)
    refocus newfoc xs = xs { windowset = W.focusWindow newfoc (windowset xs) }

-- | Refocuses the previously focused window; acts as a toggle.
toggleFocus :: X ()
toggleFocus = withFocAndRec $ \fw _ (Recent lw _) ->
  when (fw /= lw) (windows $ W.focusWindow lw)

-- | Sends the focused window to the specified workspace, refocusing the last
--   focused window. Note that the native version of this, @windows . W.shift@,
--   has a nice property that this does not: shifting a window to another
--   workspace then shifting it back preserves its place in the stack.
shiftRL :: WorkspaceId -> X ()
shiftRL to = withFocAndRec $ \fw from (Recent lw _) ->
  when (to /= from) (windows $ W.shiftWin to fw . W.focusWindow lw)

-- }}}

-- --< Private Utilities >-- {{{

-- | Get the RecentsMap out of extensible state and remove its newtype wrapper.
getRecentsMap :: X (M.Map WorkspaceId RecentWins)
getRecentsMap = XS.get >>= \(RecentsMap m) -> return m

-- | Perform an X action dependent on the focused window and current workspace.
withFocii :: (Window -> WorkspaceId -> X ()) -> X ()
withFocii f = withWindowSet $ \ws ->
  whenJust (W.peek ws) $ \w -> f w (W.currentTag ws)

-- | As above, but also dependent on successful lookup of the RecentsMap.
withFocAndRec :: (Window -> WorkspaceId -> RecentWins -> X ()) -> X ()
withFocAndRec f = withFocii $ \fw tag ->
  M.lookup tag <$> getRecentsMap >>= flip whenJust (f fw tag)

-- }}}

