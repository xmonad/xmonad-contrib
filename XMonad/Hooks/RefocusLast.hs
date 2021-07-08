{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}

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
  -- ** Predicates
  -- $Predicates
  refocusingIsActive,
  isFloat,
  -- * Actions
  toggleRefocusing,
  toggleFocus,
  swapWithLast,
  refocusWhen,
  shiftRLWhen,
  updateRecentsOn,
  -- * Types
  -- $Types
  RecentWins(..),
  RecentsMap(..),
  RefocusLastLayoutHook(..),
  RefocusLastToggle(..),
  -- * Library functions
  withRecentsIn,
) where

import XMonad
import XMonad.Prelude (All (..), asum, fromMaybe, when)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Stack (findS, mapZ_)
import XMonad.Layout.LayoutModifier

import qualified Data.Map.Strict as M

-- }}}

-- --< Usage >-- {{{

-- $Usage
-- To use this module, you must either include 'refocusLastLogHook' in your log
-- hook __or__ 'refocusLastLayoutHook' in your layout hook; don't use both.
-- This suffices to make use of both 'toggleFocus' and 'shiftRLWhen' but will
-- not refocus automatically upon loss of the current window; for that you must
-- include in your event hook @'refocusLastWhen' pred@ for some valid @pred@.
--
-- The event hooks that trigger refocusing only fire when a window is lost
-- completely, not when it's simply e.g. moved to another workspace. Hence you
-- will need to use @'shiftRLWhen' pred@ or @'refocusWhen' pred@ as appropriate
-- if you want the same behaviour in such cases.
--
-- Example configuration:
--
-- > import XMonad
-- > import XMonad.Hooks.RefocusLast
-- > import qualified Data.Map.Strict as M
-- >
-- > main :: IO ()
-- > main = xmonad def
-- >     { handleEventHook = refocusLastWhen myPred <+> handleEventHook def
-- >     , logHook         = refocusLastLogHook     <+> logHook         def
-- > --  , layoutHook      = refocusLastLayoutHook   $  layoutHook      def
-- >     , keys            = refocusLastKeys        <+> keys            def
-- >     } where
-- >         myPred = refocusingIsActive <||> isFloat
-- >         refocusLastKeys cnf
-- >           = M.fromList
-- >           $ ((modMask cnf              , xK_a), toggleFocus)
-- >           : ((modMask cnf .|. shiftMask, xK_a), swapWithLast)
-- >           : ((modMask cnf              , xK_b), toggleRefocusing)
-- >           : [ ( (modMask cnf .|. shiftMask, n)
-- >               , windows =<< shiftRLWhen myPred wksp
-- >               )
-- >             | (n, wksp) <- zip [xK_1..xK_9] (workspaces cnf)
-- >             ]
--

-- }}}

-- --< Types >-- {{{

-- $Types
-- The types and constructors used in this module are exported principally to
-- aid extensibility; typical users will have nothing to gain from this section.

-- | Data type holding onto the previous and current @Window@.
data RecentWins = Recent { previous :: !Window, current :: !Window }
  deriving (Show, Read, Eq)

-- | Newtype wrapper for a @Map@ holding the @RecentWins@ for each workspace.
--   Is an instance of @ExtensionClass@ with persistence of state.
newtype RecentsMap = RecentsMap (M.Map WorkspaceId RecentWins)
  deriving (Show, Read, Eq)

instance ExtensionClass RecentsMap where
  initialValue = RecentsMap M.empty
  extensionType = PersistentExtension

-- | A 'LayoutModifier' that updates the 'RecentWins' for a workspace upon
--   relayout.
data RefocusLastLayoutHook a = RefocusLastLayoutHook
  deriving (Show, Read)

instance LayoutModifier RefocusLastLayoutHook a where
  modifyLayout _ w@(W.Workspace tg _ _) r = updateRecentsOn tg >> runLayout w r

-- | A newtype on @Bool@ to act as a universal toggle for refocusing.
newtype RefocusLastToggle = RefocusLastToggle { refocusing :: Bool }
  deriving (Show, Read, Eq)

instance ExtensionClass RefocusLastToggle where
  initialValue  = RefocusLastToggle { refocusing = True }
  extensionType = PersistentExtension

-- }}}

-- --< Public Hooks >-- {{{

-- | A log hook recording the current workspace's most recently focused windows
--   into extensible state.
refocusLastLogHook :: X ()
refocusLastLogHook = withWindowSet (updateRecentsOn . W.currentTag)

-- | Records a workspace's recently focused windows into extensible state upon
--   relayout. Potentially a less wasteful alternative to @refocusLastLogHook@,
--   as it does not run on @WM_NAME@ @propertyNotify@ events.
refocusLastLayoutHook :: l a -> ModifiedLayout RefocusLastLayoutHook l a
refocusLastLayoutHook = ModifiedLayout RefocusLastLayoutHook

-- | Given a predicate on the event window determining whether or not to act,
--   construct an event hook that runs iff the core xmonad event handler will
--   unmanage the window, and which shifts focus to the last focused window on
--   the appropriate workspace if desired.
refocusLastWhen :: Query Bool -> Event -> X All
refocusLastWhen p event = All True <$ case event of
  UnmapEvent { ev_send_event = synth, ev_window = w } -> do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    when (synth || e == 0) (refocusLast w)
  DestroyWindowEvent {                ev_window = w } -> refocusLast w
  _                                                   -> return ()
  where
    refocusLast w = whenX (runQuery p w) . withWindowSet $ \ws ->
      whenJust (W.findTag w ws) $ \tag ->
        withRecentsIn tag () $ \lw cw ->
          when (w == cw) . modify $ \xs ->
            xs { windowset = tryFocusIn tag [lw] ws }

-- }}}

-- --< Predicates >-- {{{

-- $Predicates
-- Impure @Query Bool@ predicates on event windows for use as arguments to
-- 'refocusLastWhen', 'shiftRLWhen' and 'refocusWhen'. Can be combined with
-- '<||>' or '<&&>'. Use like e.g.
--
-- > , handleEventHook = refocusLastWhen refocusingIsActive
--
-- or in a keybinding:
--
-- > windows =<< shiftRLWhen (refocusingIsActive <&&> isFloat) "3"
--
-- It's also valid to use a property lookup like @className =? "someProgram"@ as
-- a predicate, and it should function as expected with e.g. @shiftRLWhen@.
-- In the event hook on the other hand, the window in question has already been
-- unmapped or destroyed, so external lookups to X properties don't work:
-- only the information fossilised in xmonad's state is available.

-- | Holds iff refocusing is toggled active.
refocusingIsActive :: Query Bool
refocusingIsActive = (liftX . XS.gets) refocusing

-- | Holds iff the event window is a float.
isFloat :: Query Bool
isFloat = ask >>= \w -> (liftX . gets) (M.member w . W.floating . windowset)

-- }}}

-- --< Public Actions >-- {{{

-- | Toggle automatic refocusing at runtime. Has no effect unless the
--   @refocusingIsActive@ predicate has been used.
toggleRefocusing :: X ()
toggleRefocusing = XS.modify (RefocusLastToggle . not . refocusing)

-- | Refocuses the previously focused window; acts as a toggle.
--   Is not affected by @toggleRefocusing@.
toggleFocus :: X ()
toggleFocus = withRecents $ \lw cw ->
  when (cw /= lw) . windows $ tryFocus [lw]

-- | Swaps the current and previous windows of the current workspace.
--   Is not affected by @toggleRefocusing@.
swapWithLast :: X ()
swapWithLast = withRecents $ \lw cw ->
  when (cw /= lw) . windows . modify''. mapZ_ $ \w ->
    if | (w == lw) -> cw
       | (w == cw) -> lw
       | otherwise ->  w
  where modify'' f = W.modify (f Nothing) (f . Just)

-- | Given a target workspace and a predicate on its current window, produce a
--   'windows' suitable function that will refocus that workspace appropriately.
--   Allows you to hook refocusing into any action you can run through
--   @windows@. See the implementation of @shiftRLWhen@ for a straight-forward
--   usage example.
refocusWhen :: Query Bool -> WorkspaceId -> X (WindowSet -> WindowSet)
refocusWhen p tag = withRecentsIn tag id $ \lw cw -> do
  b <- runQuery p cw
  return (if b then tryFocusIn tag [cw, lw] else id)

-- | Sends the focused window to the specified workspace, refocusing the last
--   focused window if the predicate holds on the current window. Note that the
--   native version of this, @windows . W.shift@, has a nice property that this
--   does not: shifting a window to another workspace then shifting it back
--   preserves its place in the stack. Can be used in a keybinding like e.g.
--
-- > windows =<< shiftRLWhen refocusingIsActive "3"
--
--   or
--
-- > (windows <=< shiftRLWhen refocusingIsActive) "3"
--
--   where '<=<' is imported from "Control.Monad".
shiftRLWhen :: Query Bool -> WorkspaceId -> X (WindowSet -> WindowSet)
shiftRLWhen p to = withWindowSet $ \ws -> do
  refocus <- refocusWhen p (W.currentTag ws)
  let shift = maybe id (W.shiftWin to) (W.peek ws)
  return (refocus . shift)

-- | Perform an update to the 'RecentWins' for the specified workspace.
--   The RefocusLast log and layout hooks are both implemented trivially in
--   terms of this function. Only exported to aid extensibility.
updateRecentsOn :: WorkspaceId -> X ()
updateRecentsOn tag = withWindowSet $ \ws ->
  whenJust (W.peek $ W.view tag ws) $ \fw -> do
    m <- getRecentsMap
    let insertRecent l c = XS.put . RecentsMap $ M.insert tag (Recent l c) m
    case M.lookup tag m of
      Just (Recent _ cw) -> when (cw /= fw) (insertRecent cw fw)
      Nothing            ->                  insertRecent fw fw

-- }}}

-- --< Utilities >-- {{{

-- | Focuses the first window in the list it can find on the current workspace.
tryFocus :: [Window] -> WindowSet -> WindowSet
tryFocus wins = W.modify' $ \s ->
  fromMaybe s . asum $ (\w -> findS (== w) s) <$> wins

-- | Operate the above on a specified workspace.
tryFocusIn :: WorkspaceId -> [Window] -> WindowSet -> WindowSet
tryFocusIn tag wins ws =
  W.view (W.currentTag ws) . tryFocus wins . W.view tag $ ws

-- | Get the RecentsMap out of extensible state and remove its newtype wrapper.
getRecentsMap :: X (M.Map WorkspaceId RecentWins)
getRecentsMap = XS.get >>= \(RecentsMap m) -> return m

-- | Perform an X action dependent on successful lookup of the RecentWins for
--   the specified workspace, or return a default value.
withRecentsIn :: WorkspaceId -> a -> (Window -> Window -> X a) -> X a
withRecentsIn tag dflt f = maybe (return dflt) (\(Recent lw cw) -> f lw cw)
                         . M.lookup tag
                       =<< getRecentsMap

-- | The above specialised to the current workspace and unit.
withRecents :: (Window -> Window -> X ()) -> X ()
withRecents f = withWindowSet $ \ws -> withRecentsIn (W.currentTag ws) () f

-- }}}
