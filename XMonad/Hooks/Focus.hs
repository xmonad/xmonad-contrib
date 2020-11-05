{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      XMonad.Hooks.Focus
-- Copyright:   sgf-dma, 2016
-- Maintainer:  sgf.dma@gmail.com
--

module XMonad.Hooks.Focus
    (
      -- $main

      -- * FocusQuery.
      --
      -- $focusquery
      Focus (..)
    , FocusLock (..)
    , toggleLock
    , focusLockOn
    , focusLockOff
    , FocusQuery
    , runFocusQuery
    , FocusHook

      -- * Lifting into FocusQuery.
      --
      -- $lift
    , liftQuery
    , new
    , focused
    , focused'
    , focusedOn
    , focusedOn'
    , focusedCur
    , focusedCur'
    , newOn
    , newOnCur
    , unlessFocusLock

      -- * Commonly used actions for modifying focus.
      --
      -- $common
    , keepFocus
    , switchFocus
    , keepWorkspace
    , switchWorkspace

      -- * Running FocusQuery.
      --
      -- $running
    , manageFocus

      -- * Example configurations.
      --
      -- $examples
    {-, activateSwitchWs
    , activateOnCurrentWs
    , activateOnCurrentKeepFocus-}
    )
  where

import Control.Arrow ((&&&))
import Control.Monad.Reader

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.ManageHelpers (currentWs)
-- import XMonad.Hooks.EwmhDesktops (activated)


-- $main
--
-- This module provides monad on top of Query monad providing additional
-- information about new window:
--
--  - workspace, where new window will appear;
--  - focused window on workspace, where new window will appear;
--  - current workspace;
--
-- And a property in extensible state:
--
--  - is focus lock enabled? Focus lock instructs all library's 'FocusHook'
--  functions to not move focus or switch workspace.
--
-- Lifting operations for standard 'ManageHook' EDSL combinators into
-- 'FocusQuery' monad allowing to run these combinators on focused window and
-- common actions for keeping focus and\/or workspace, switching focus and\/or
-- workspace are also provided.
--
-- == Quick start.
--
-- I may use one of predefined configurations.
--
-- 1. Default window activation behavior is to switch to workspace with
--    activated window and switch focus to it:
--
--      > import XMonad
--      >
--      > import XMonad.Hooks.EwmhDesktops
--      > import XMonad.Hooks.Focus
--      >
--      > main :: IO ()
--      > main = do
--      >         let mh :: ManageHook
--      >             mh = activateSwitchWs
--      >             xcf = ewmh $ def
--      >                     { modMask = mod4Mask
--      >                     , logHook = activateLogHook mh <+> logHook def
--      >                     }
--      >         xmonad xcf
--
-- 2. Or i may move activated window to current workspace and switch focus to
--    it:
--
--      >         let mh :: ManageHook
--      >             mh = activateOnCurrentWs
--
-- 3. Or move activated window to current workspace, but keep focus unchanged:
--
--      >         let mh :: ManageHook
--      >             mh = activateOnCurrentKeepFocus
--
-- 4. I may use regular 'ManageHook' combinators for filtering, which windows
--    may activate. E.g. activate all windows, except firefox:
--
--      >         let mh :: ManageHook
--      >             mh  = not <$> (className =? "Firefox" <||> className =? "Firefox-esr" <||> className =? "Iceweasel")
--      >                     --> activateSwitchWs
--
-- 5. Or even use 'FocusHook' combinators. E.g. activate all windows, unless
--    xterm is focused on /current/ workspace:
--
--      >         let mh :: ManageHook
--      >             mh  = manageFocus (not <$> focusedCur (className =? "XTerm")
--      >                     --> liftQuery activateSwitchWs)
--
--      or activate all windows, unless focused window on the workspace,
--      /where activated window is/, is not a xterm:
--
--      >         let mh :: ManageHook
--      >             mh  = manageFocus (not <$> focused (className =? "XTerm")
--      >                     --> liftQuery activateSwitchWs)
--
-- == Defining FocusHook.
--
-- I may define my own 'FocusHook' like:
--
-- >    activateFocusHook :: FocusHook
-- >    activateFocusHook = composeAll
-- >            -- If 'gmrun' is focused on workspace, on which
-- >            -- /activated window/ is, keep focus unchanged. But i
-- >            -- may still switch workspace (thus, i use 'composeAll').
-- >            -- See 'keepFocus' properties in the docs below.
-- >            [ focused (className =? "Gmrun")
-- >                            --> keepFocus
-- >            -- Default behavior for activated windows: switch
-- >            -- workspace and focus.
-- >            , return True   --> switchWorkspace <+> switchFocus
-- >            ]
-- >
-- >    newFocusHook :: FocusHook
-- >    newFocusHook      = composeOne
-- >            -- Always switch focus to 'gmrun'.
-- >            [ new (className =? "Gmrun")        -?> switchFocus
-- >            -- And always keep focus on 'gmrun'. Note, that
-- >            -- another 'gmrun' will steal focus from already
-- >            -- running one.
-- >            , focused (className =? "Gmrun")    -?> keepFocus
-- >            -- If firefox dialog prompt (e.g. master password
-- >            -- prompt) is focused on current workspace and new
-- >            -- window appears here too, keep focus unchanged
-- >            -- (note, used predicates: @newOnCur <&&> focused@ is
-- >            -- the same as @newOnCur <&&> focusedCur@, but is
-- >            -- /not/ the same as just 'focusedCur' )
-- >            , newOnCur <&&> focused
-- >                ((className =? "Firefox" <||> className =? "Firefox-esr" <||> className =? "Iceweasel") <&&> isDialog)
-- >                                                -?> keepFocus
-- >            -- Default behavior for new windows: switch focus.
-- >            , return True                       -?> switchFocus
-- >            ]
--
-- And then use it:
--
-- >    import XMonad
-- >    import XMonad.Util.EZConfig
-- >
-- >    import XMonad.Hooks.EwmhDesktops
-- >    import XMonad.Hooks.ManageHelpers
-- >    import XMonad.Hooks.Focus
-- >
-- >
-- >    main :: IO ()
-- >    main = do
-- >            let newFh :: ManageHook
-- >                newFh = manageFocus newFocusHook
-- >                acFh :: X ()
-- >                acFh = activateLogHook (manageFocus activateFocusHook)
-- >                xcf = ewmh $ def
-- >                             { manageHook   = newFh <+> manageHook def
-- >                             , logHook      = acFh  <+> logHook def
-- >                             , modMask      = mod4Mask
-- >                             }
-- >                        `additionalKeys` [((mod4Mask, xK_v), toggleLock)]
-- >            xmonad xcf
--
-- Note:
--
--  - /mod4Mask+v/ key toggles focus lock (when enabled, neither focus nor
--  workspace won't be switched).
--  - I need 'XMonad.Hooks.EwmhDesktops' module for enabling window
--  activation.
--  - 'FocusHook' in 'manageHook' will be called /only/ for new windows.
--  - 'FocusHook' in 'logHook' will be called /only/ for activated windows.
--
--  Alternatively, i may construct a single 'FocusHook' for both new and
--  activated windows and then just add it to both 'manageHook' and 'logHook':
--
-- >            let fh :: ManageHook
-- >                fh = manageFocus $ (composeOne
-- >                        [ liftQuery activated -?> activateFocusHook
-- >                        , Just <$> newFocusHook
-- >                        ])
-- >                xcf = ewmh $ def
-- >                             { manageHook   = fh <+> manageHook def
-- >                             , logHook      = activateLogHook fh <+> logHook def
-- >                             , modMask      = mod4Mask
-- >                             }
-- >                        `additionalKeys` [((mod4Mask, xK_v), toggleLock)]
--
-- Note:
--  - Predicate 'activated' will be 'True' for activated window.
--  - The order, when constructing final 'FocusHook': 'FocusHook' without
--  'activated' predicate will match to activated windows too, thus i should
--  place it after one with 'activated' (so the latter will have a chance to
--  handle activated window first).
--
-- And more technical notes:
--
--  - 'FocusHook' will run /many/ times, so it usually should not keep state
--  or save results. Precisely, it may do anything, but it must be idempotent
--  to operate properly.
--  - 'FocusHook' will see new window at workspace, where functions on the
--  /right/ from it in 'ManageHook' monoid place it.  In other words, in
--  @(Endo WindowSet)@ monoid i may see changes only from functions applied
--  /before/ (more to the right in function composition). Thus, it's better to
--  add 'FocusHook' the last.
--  - 'FocusHook' functions won't see window shift to another workspace made
--  by function from 'FocusHook' itself: new window workspace is determined
--  /before/ running 'FocusHook' and even if later one of 'FocusHook'
--  functions moves window to another workspace, predicates ('focused',
--  'newOn', etc) will still think new window is at workspace it was before.
--  This can be worked around by splitting 'FocusHook' into several different
--  values and evaluating each one separately, like:
--
--      > (FH2 -- manageFocus --> MH2) <+> (FH1 -- manageFocus --> MH1) <+> ..
--
--      E.g.
--
--      > manageFocus FH2 <+> manageFocus FH1 <+> ..
--
--      now @FH2@ will see window shift made by @FH1@.
--
--      Also, note, that if several 'activateLogHook'-s are sequenced, only
--      /first/ one (leftmost) will run. Thus, to make above working,
--      'mappend' all 'ManageHook'-s first, and then run by /single/
--      'activateLogHook' (see next example).
--
-- Another interesting example is moving all activated windows to current
-- workspace by default, and applying 'FocusHook' after:
--
-- >    import XMonad
-- >    import XMonad.Util.EZConfig
-- >
-- >    import XMonad.Hooks.EwmhDesktops
-- >    import XMonad.Hooks.ManageHelpers
-- >    import XMonad.Hooks.Focus
-- >
-- >    main :: IO ()
-- >    main = do
-- >            let fh :: ManageHook
-- >                fh = manageFocus $ (composeOne
-- >                        [ liftQuery activated -?> (newOnCur --> keepFocus)
-- >                        , Just <$> newFocusHook
-- >                        ])
-- >                xcf = ewmh $ def
-- >                             { manageHook = fh <+> manageHook def
-- >                             , logHook    = activateLogHook (fh <+> activateOnCurrentWs) <+> logHook def
-- >                             , modMask    = mod4Mask
-- >                             }
-- >                        `additionalKeys` [((mod4Mask, xK_v), toggleLock)]
-- >            xmonad xcf
-- >
-- >    newFocusHook :: FocusHook
-- >    newFocusHook      = composeOne
-- >            -- Always switch focus to 'gmrun'.
-- >            [ new (className =? "Gmrun")        -?> switchFocus
-- >            -- And always keep focus on 'gmrun'. Note, that
-- >            -- another 'gmrun' will steal focus from already
-- >            -- running one.
-- >            , focused (className =? "Gmrun")    -?> keepFocus
-- >            -- If firefox dialog prompt (e.g. master password
-- >            -- prompt) is focused on current workspace and new
-- >            -- window appears here too, keep focus unchanged
-- >            -- (note, used predicates: @newOnCur <&&> focused@ is
-- >            -- the same as @newOnCur <&&> focusedCur@, but is
-- >            -- /not/ the same as just 'focusedCur' )
-- >            , newOnCur <&&> focused
-- >                ((className =? "Firefox" <||> className =? "Firefox-esr" <||> className =? "Iceweasel") <&&> isDialog)
-- >                                                -?> keepFocus
-- >            -- Default behavior for new windows: switch focus.
-- >            , return True                       -?> switchFocus
-- >            ]
--
-- Note here:
--
--  - i keep focus, when activated window appears on current workspace, in
--  this example.
--  - when @liftQuery activated -?> (newOnCur --> keepFocus)@ runs, activated
--  window will be /already/ on current workspace, thus, if i do not want to
--  move some activated windows, i should filter them out before applying
--  @activateOnCurrentWs@ 'FocusHook'.
--  - i 'mappend' all 'ManageHook'-s and run 'activateLogHook' only once.


-- FocusQuery.
-- $focusquery

-- | Information about current workspace and focus.
data Focus          = Focus
                        { -- | Workspace, where new window appears.
                          newWorkspace      :: WorkspaceId
                          -- | Focused window on workspace, where new window
                          -- appears.
                        , focusedWindow     :: Maybe Window
                          -- | Current workspace.
                        , currentWorkspace  :: WorkspaceId
                        }
  deriving (Show)

-- TODO: drop this
instance Default Focus where
    def             = Focus
                        { focusedWindow     = Nothing
                        , newWorkspace      = ""
                        , currentWorkspace  = ""
                        }

newtype FocusLock   = FocusLock {getFocusLock :: Bool}
  deriving (Show, Typeable)
instance ExtensionClass FocusLock where
    initialValue    = FocusLock False

-- | Toggle stored focus lock state.
toggleLock :: X ()
toggleLock          = XS.modify (\(FocusLock b) -> FocusLock (not b))

-- | Lock focus.
focusLockOn :: X ()
focusLockOn         = XS.modify (const (FocusLock True))

-- | Unlock focus.
focusLockOff :: X ()
focusLockOff        = XS.modify (const (FocusLock False))

-- | Monad on top of 'Query' providing additional information about new
-- window.
newtype FocusQuery a = FocusQuery (ReaderT Focus Query a)
instance Functor FocusQuery where
    fmap f (FocusQuery x) = FocusQuery (fmap f x)
instance Applicative FocusQuery where
    pure x                              = FocusQuery (pure x)
    (FocusQuery f) <*> (FocusQuery mx)  = FocusQuery (f <*> mx)
instance Monad FocusQuery where
    return x                = FocusQuery (return x)
    (FocusQuery mx) >>= f   = FocusQuery $ mx >>= \x ->
                              let FocusQuery y = f x in y
instance MonadReader Focus FocusQuery where
    ask                     = FocusQuery ask
    local f (FocusQuery mx) = FocusQuery (local f mx)
instance MonadIO FocusQuery where
    liftIO mx       = FocusQuery (liftIO mx)
instance Semigroup a => Semigroup (FocusQuery a) where
    (<>)            = liftM2 (<>)
instance Monoid a => Monoid (FocusQuery a) where
    mempty          = return mempty
    mappend         = (<>)

runFocusQuery :: FocusQuery a -> Focus -> Query a
runFocusQuery (FocusQuery m)    = runReaderT m

type FocusHook      = FocusQuery (Endo WindowSet)


-- Lifting into 'FocusQuery'.
-- $lift

-- | Lift 'Query' into 'FocusQuery' monad. The same as 'new'.
liftQuery :: Query a -> FocusQuery a
liftQuery           = FocusQuery . lift

-- | Run 'Query' on new window.
new :: Query a -> FocusQuery a
new                 = liftQuery

-- | Run 'Query' on focused window on workspace, where new window appears. If
-- there is no focused window, return 'False'.
focused :: Query Bool -> FocusQuery Bool
focused m           = getAny <$> focused' (Any <$> m)
-- | More general version of 'focused'.
focused' :: Monoid a => Query a -> FocusQuery a
focused' m          = do
    mw <- asks focusedWindow
    liftQuery (maybe mempty (flip local m . const) mw)

-- | Run 'Query' on window focused at particular workspace. If there is no
-- focused window, return 'False'.
focusedOn :: WorkspaceId -> Query Bool -> FocusQuery Bool
focusedOn i m       = getAny <$> focusedOn' i (Any <$> m)
-- | More general version of 'focusedOn'.
focusedOn' :: Monoid a => WorkspaceId -> Query a -> FocusQuery a
focusedOn' i m      = liftQuery $ do
    mw <- liftX $ withWindowSet (return . W.peek . W.view i)
    maybe mempty (flip local m . const) mw

-- | Run 'Query' on focused window on current workspace. If there is no
-- focused window, return 'False'.  Note,
--
-- > focused <&&> newOnCur != focusedCur
--
-- The first will affect only new or activated window appearing on current
-- workspace, while the last will affect any window: focus even for windows
-- appearing on other workpsaces will depend on focus on /current/ workspace.
focusedCur :: Query Bool -> FocusQuery Bool
focusedCur m        = getAny <$> focusedCur' (Any <$> m)
-- | More general version of 'focusedCur'.
focusedCur' :: Monoid a => Query a -> FocusQuery a
focusedCur' m       = asks currentWorkspace >>= \i -> focusedOn' i m

-- | Does new window appear at particular workspace?
newOn :: WorkspaceId -> FocusQuery Bool
newOn i             = (i ==) <$> asks newWorkspace
-- | Does new window appear at current workspace?
newOnCur :: FocusQuery Bool
newOnCur            = asks currentWorkspace >>= newOn

-- | Execute 'Query', unless focus is locked.
unlessFocusLock :: Monoid a => Query a -> Query a
unlessFocusLock m   = do
    FocusLock b <- liftX XS.get
    when' (not b) m

-- Commonly used actions for modifying focus.
--
-- $common
-- Operations in each pair 'keepFocus' and 'switchFocus', 'keepWorkspace' and
-- 'switchWorkspace' overwrite each other (the letftmost will determine what
-- happened):
--
-- prop> keepFocus       <+> switchFocus     = keepFocus
-- prop> switchFocus     <+> keepFocus       = switchFocus
-- prop> keepWorkspace   <+> switchWorkspace = keepWorkspace
-- prop> switchWorkspace <+> keepWorkspace   = switchWorkspace
--
-- and operations from different pairs are commutative:
--
-- prop> keepFocus   <+> switchWorkspace = switchWorkspace <+> keepFocus
-- prop> switchFocus <+> switchWorkspace = switchWorkspace <+> switchFocus
--
-- etc.

-- | Keep focus on workspace (may not be current), where new window appears.
-- Workspace will not be switched. This operation is idempotent and
-- effectively returns focus to window focused on that workspace before
-- applying @(Endo WindowSet)@ function.
keepFocus :: FocusHook
keepFocus           = focused' $ ask >>= \w -> doF $ \ws ->
                        W.view (W.currentTag ws) . W.focusWindow w $ ws

-- | Switch focus to new window on workspace (may not be current), where new
-- window appears. Workspace will not be switched. This operation is
-- idempotent.
switchFocus :: FocusHook
switchFocus         = do
    FocusLock b <- liftQuery . liftX $ XS.get
    if b
      -- When focus lock is enabled, call 'keepFocus' explicitly (still no
      -- 'keepWorkspace') to overwrite default behavior.
      then keepFocus
      else new $ ask >>= \w -> doF $ \ws ->
            W.view (W.currentTag ws) . W.focusWindow w $ ws

-- | Keep current workspace. Focus will not be changed at either current or
-- new window's  workspace. This operation is idempotent and effectively
-- switches to workspace, which was current before applying @(Endo WindowSet)@
-- function.
keepWorkspace :: FocusHook
keepWorkspace       = do
    ws <- asks currentWorkspace
    liftQuery . doF $ W.view ws

-- | Switch workspace to one, where new window appears. Focus will not be
-- changed at either current or new window's workspace. This operation is
-- idempotent.
switchWorkspace :: FocusHook
switchWorkspace     = do
    FocusLock b <- liftQuery . liftX $ XS.get
    if b
      -- When focus lock is enabled, call 'keepWorkspace' explicitly (still no
      -- 'keepFocus') to overwrite default behavior.
      then keepWorkspace
      else do
        ws <- asks newWorkspace
        liftQuery . doF $ W.view ws

-- Running FocusQuery.
-- $running

-- | I don't know at which workspace new window will appear until @(Endo
-- WindowSet)@ function from 'windows' in "XMonad.Operations" actually run,
-- but in @(Endo WindowSet)@ function i can't already execute monadic actions,
-- because it's pure. So, i compute result for every workspace here and just
-- use it later in @(Endo WindowSet)@ function.  Note, though, that this will
-- execute monadic actions many times, and therefore assume, that result of
-- 'FocusHook' does not depend on the number of times it was executed.
manageFocus :: FocusHook -> ManageHook
manageFocus m       = do
    fws <- liftX . withWindowSet $ return
      . map (W.tag &&& fmap W.focus . W.stack) . W.workspaces
    ct  <- currentWs
    let r = def {currentWorkspace = ct}
    hs <- forM fws $ \(i, mw) -> do
      f <- runFocusQuery m (r {focusedWindow = mw, newWorkspace = i})
      return (i, f)
    reader (selectHook hs) >>= doF
  where
    -- | Select and apply @(Endo WindowSet)@ function depending on which
    -- workspace new window appeared now.
    selectHook :: [(WorkspaceId, Endo WindowSet)] -> Window -> WindowSet -> WindowSet
    selectHook cfs nw ws    = fromMaybe ws $ do
        i <- W.findTag nw ws
        f <- lookup i cfs
        return (appEndo f ws)

when' :: (Monad m, Monoid a) => Bool -> m a -> m a
when' b mx
  | b               = mx
  | otherwise       = return mempty

-- Exmaple configurations.
-- $examples

{-
-- | Default EWMH window activation behavior: switch to workspace with
-- activated window and switch focus to it.
activateSwitchWs :: ManageHook
activateSwitchWs    = manageFocus (liftQuery activated -->
                        switchWorkspace <+> switchFocus)

-- | Move activated window to current workspace.
activateOnCurrent' :: ManageHook
activateOnCurrent'  = activated --> currentWs >>= unlessFocusLock . doShift

-- | Move activated window to current workspace and switch focus to it. Note,
-- that i need to explicitly call 'switchFocus' here, because otherwise, when
-- activated window is /already/ on current workspace, focus won't be
-- switched.
activateOnCurrentWs :: ManageHook
activateOnCurrentWs = manageFocus (liftQuery activated <&&> newOnCur --> switchFocus)
                        <+> activateOnCurrent'

-- | Move activated window to current workspace, but keep focus unchanged.
activateOnCurrentKeepFocus :: ManageHook
activateOnCurrentKeepFocus  = manageFocus (liftQuery activated <&&> newOnCur --> keepFocus)
                        <+> activateOnCurrent'
-}
