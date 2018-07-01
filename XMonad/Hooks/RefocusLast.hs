{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

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
-- Provides hooks and actions that keep track of recently focused windows on a
-- per workspace basis and automatically refocus the last window on loss of the
-- current (if appropriate as determined by user specified criteria).
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

module XMonad.Hooks.RefocusLast (
  -- * Usage
  -- $Usage
  -- * Hooks
  refocusLastLogHook,
  refocusLastLayoutHook,
  refocusLastWhen,
  alwaysRefocusLastEvHook,
  refocusFromFloatsEvHook,
  -- * Actions
  toggleFocus,
  refocus,
  shiftRL,
  updateRecentsOn,
  -- * Types
  RecentWins(..),
  RecentsMap(..),
  RefocusLastLayoutHook(..)
) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Stack (findS)
import XMonad.Layout.LayoutModifier

import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Control.Monad (when)

-- }}}

-- --< Usage >-- {{{

-- $Usage
-- To use this module, you must either include 'refocusLastLogHook' in your log
-- hook __or__ 'refocusLastLayoutHook' in your layout hook; don't use both.
-- This suffices to make use of both 'toggleFocus' and 'shiftRL' but will not
-- refocus automatically upon loss of the current window; for that you must
-- include in your event hook exactly one of 'alwaysRefocusLastEvHook',
-- 'refocusFromFloatsEvHook' or @'refocusLastWhen' pred@ for some valid @pred@.
--
-- The event hooks that trigger refocusing only fire when a window is lost
-- completely, not when it's simply moved to another workspace. Hence you will
-- need to use 'shiftRL' or 'refocus' as appropriate if you want the same
-- behaviour in such a case.
--
-- Example configuration:
--
-- > import XMonad
-- > import XMonad.Hooks.RefocusLast
-- > import qualified Data.Map.Strict as M
-- >
-- > main :: IO ()
-- > main = xmonad def
-- >     { handleEventHook = alwaysRefocusLastEvHook <+> handleEventHook def
-- > --  { handleEventHook = refocusFromFloatsEvHook <+> handleEventHook def
-- >     , logHook         = refocusLastLogHook      <+> logHook         def
-- > --  , layoutHook      = refocusLastLayoutHook    $  layoutHook      def
-- >     , keys            = rlKeys                  <+> keys            def
-- >     } where
-- >         rlKeys
-- >           = \cnf -> M.fromList
-- >           $   ((modMask cnf              , xK_a), toggleFocus)
-- >           : [ ((modMask cnf .|. shiftMask, n   ), windows =<< shiftRL wksp)
-- >             | (n, wksp) <- zip [xK_1..xK_9] (workspaces cnf)
-- >             ]
--

-- }}}

-- --< Types >-- {{{

-- | Data type holding onto the previous and current @Window@.
data RecentWins = Recent { previous :: !Window, current :: !Window }
  deriving (Show, Read, Eq, Typeable)

-- | Newtype wrapper for a @Map@ holding the @RecentWins@ for each workspace.
--   Is an instance of @ExtensionClass@ with persistence of state.
newtype RecentsMap = RecentsMap (M.Map WorkspaceId RecentWins)
  deriving (Show, Read, Eq, Typeable)

instance ExtensionClass RecentsMap where
  initialValue = RecentsMap M.empty
  extensionType = PersistentExtension

-- | A 'LayoutModifier' that updates the 'RecentWins' for a workspace upon
--   relayout.
data RefocusLastLayoutHook a = RefocusLastLayoutHook
  deriving (Show, Read)

instance LayoutModifier RefocusLastLayoutHook a where
  modifyLayout _ w@(W.Workspace tag _ _) r = do
    updateRecentsOn tag
    runLayout w r

-- }}}

-- --< Public Hooks >-- {{{

-- | A log hook recording the current workspace's most recently focused windows
--   into extensible state.
refocusLastLogHook :: X ()
refocusLastLogHook = updateRecentsOn =<< gets (W.currentTag . windowset)

-- | Records a workspace's recently focused windows into extensible state upon
--   relayout. Potentially a less wasteful alternative to @refocusLastLogHook@,
--   as it does not run on @WM_NAME@ @propertyNotify@ events.
refocusLastLayoutHook :: l a -> ModifiedLayout RefocusLastLayoutHook l a
refocusLastLayoutHook = ModifiedLayout RefocusLastLayoutHook

-- | Given a predicate on the event window determining whether or not to act,
--   construct an event hook that runs iff the core xmonad event handler will
--   unmanage the window, and which shifts focus to the last focused window on
--   the appropriate workspace if desired.
refocusLastWhen :: (Window -> X Bool) -> Event -> X All
refocusLastWhen p event = All True <$ case event of
  UnmapEvent { ev_send_event = synth, ev_window = w } -> do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    when (synth || e == 0) (refocusLast w)
  DestroyWindowEvent {                ev_window = w } -> refocusLast w
  _                                                   -> return ()
  where
    refocusLast w = whenX (p w) . withWindowSet $ \ws ->
      whenJust (W.findTag w ws) $ \tag ->
        withRecentsIn tag () $ \lw cw ->
          when (w == cw) . modify $ \xs ->
            xs { windowset = tryFocusIn tag [lw] ws }

-- | Always refocus the last window.
alwaysRefocusLastEvHook :: Event -> X All
alwaysRefocusLastEvHook = refocusLastWhen $ \_ ->
  return True

-- | Refocus from floating windows only.
refocusFromFloatsEvHook :: Event -> X All
refocusFromFloatsEvHook = refocusLastWhen $ \w ->
  gets (M.member w . W.floating . windowset)

-- }}}

-- --< Public Actions >-- {{{

-- | Refocuses the previously focused window; acts as a toggle.
toggleFocus :: X ()
toggleFocus = withFocii () $ \_ tag ->
  withRecentsIn tag () $ \lw cw ->
    when (cw /= lw) (windows $ tryFocusInCurrent [lw])

-- | Given a target workspace, produce a 'windows' suitable function that will
--   refocus that workspace appropriately. Allows you to hook refocusing into
--   any action you can run through @windows@. @shiftRL@ is implemented
--   straight-forwardly in terms of this.
refocus :: WorkspaceId -> X (WindowSet -> WindowSet)
refocus tag = withRecentsIn tag id $ \lw cw -> return (tryFocusIn tag [cw, lw])

-- | Sends the focused window to the specified workspace, refocusing the last
--   focused window. Note that the native version of this, @windows . W.shift@,
--   has a nice property that this does not: shifting a window to another
--   workspace then shifting it back preserves its place in the stack. Can be
--   used in a keybinding like e.g.
--
-- > windows =<< shiftRL "3"
--
--   or
--
-- > (windows <=< shiftRL) "3"
--
shiftRL :: WorkspaceId -> X (WindowSet -> WindowSet)
shiftRL to = withFocii id $ \fw from -> do
  f <- refocus from
  return (f . W.shiftWin to fw)

-- | Perform an update to the 'RecentWins' for the specified workspace.
--   The RefocusLast log and layout hooks are both implemented trivially in
--   terms of this function.
updateRecentsOn :: WorkspaceId -> X ()
updateRecentsOn tag = withWindowSet $ \ws ->
  whenJust (W.peek $ W.view tag ws) $ \fw -> do
    m <- getRecentsMap
    let insertRecent l c = XS.put . RecentsMap $ M.insert tag (Recent l c) m
    case M.lookup tag m of
      Just (Recent _ cw) -> when (cw /= fw) (insertRecent cw fw)
      Nothing            ->                  insertRecent fw fw

-- }}}

-- --< Private Utilities >-- {{{

-- | Focuses the first window in the list it can find on the current workspace.
tryFocusInCurrent :: [Window] -> WindowSet -> WindowSet
tryFocusInCurrent wins = W.modify' $ \s ->
  fromMaybe s . asum $ (\w -> findS (== w) s) <$> wins

-- | Operate the above on a specified workspace.
tryFocusIn :: WorkspaceId -> [Window] -> WindowSet -> WindowSet
tryFocusIn tag wins ws =
  W.view (W.currentTag ws) . tryFocusInCurrent wins . W.view tag $ ws

-- | Get the RecentsMap out of extensible state and remove its newtype wrapper.
getRecentsMap :: X (M.Map WorkspaceId RecentWins)
getRecentsMap = XS.get >>= \(RecentsMap m) -> return m

-- | Given a default return value, perform an X action dependent on the focused
--   window and current workspace.
withFocii :: a -> (Window -> WorkspaceId -> X a) -> X a
withFocii a f = withWindowSet $ \ws ->
  maybe (return a) (\w -> f w $ W.currentTag ws) (W.peek ws)

-- | Perform an X action dependent on successful lookup of the RecentWins for
--   the specified workspace, or return a default value.
withRecentsIn :: WorkspaceId -> a -> (Window -> Window -> X a) -> X a
withRecentsIn tag dflt f = M.lookup tag <$> getRecentsMap
                       >>= maybe (return dflt) (\(Recent lw cw) -> f lw cw)

-- }}}

