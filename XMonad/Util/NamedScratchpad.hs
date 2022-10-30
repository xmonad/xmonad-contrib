{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.NamedScratchpad
-- Description :  Toggle arbitrary windows to and from the current workspace.
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
  scratchpadWorkspaceTag,
  nonFloating,
  defaultFloating,
  customFloating,
  NamedScratchpads,
  namedScratchpadAction,
  spawnHereNamedScratchpadAction,
  customRunNamedScratchpadAction,
  allNamedScratchpadAction,
  namedScratchpadManageHook,
  nsHideOnFocusLoss,

  -- * Dynamic Scratchpads
  -- $dynamic-scratchpads
  dynamicNSPAction,
  toggleDynamicNSP,

  -- * Exclusive Scratchpads
  -- $exclusive-scratchpads
  addExclusives,
  -- ** Keyboard related
  resetFocusedNSP,
  -- ** Mouse related
  setNoexclusive,
  resizeNoexclusive,
  floatMoveNoexclusive,

  -- * Deprecations
  namedScratchpadFilterOutWorkspace,
  namedScratchpadFilterOutWorkspacePP,

  ) where

import Data.Map.Strict (Map, (!?))
import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.TagWindows (addTag, delTag)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.RefocusLast (withRecentsIn)
import XMonad.Hooks.StatusBar.PP (PP, ppSort)
import XMonad.Prelude (appEndo, filterM, findM, foldl', for_, unless, void, when, (<=<))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- Allows to have several floating scratchpads running different applications.
-- Bind a key to 'namedScratchpadAction'.
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
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>
--
-- For some applications (like displaying your workspaces in a status bar) it
-- is convenient to filter out the @NSP@ workspace when looking at all
-- workspaces. For this, you can use 'XMonad.Hooks.StatusBar.PP.filterOutWsPP',
-- or 'XMonad.Util.WorkspaceCompare.filterOutWs' together with
-- 'XMonad.Hooks.EwmhDesktops.addEwmhWorkspaceSort' if your status bar gets
-- the list of workspaces from EWMH.  See the documentation of these functions
-- for examples.
--
-- Further, there is also a @logHook@ that you can use to hide
-- scratchpads when they lose focus; this is functionality akin to what
-- some dropdown terminals provide.  See the documentation of
-- 'nsHideOnFocusLoss' for an example how to set this up.
--

-- | Single named scratchpad configuration
data NamedScratchpad = NS { name   :: String      -- ^ Scratchpad name
                          , cmd    :: String      -- ^ Command used to run application
                          , query  :: Query Bool  -- ^ Query to find already running application
                          , hook   :: ManageHook  -- ^ Manage hook called for application window, use it to define the placement. See @nonFloating@, @defaultFloating@ and @customFloating@
                          }

-- | The NSP state.
data NSPState = NSPState
  { nspExclusives  :: !(Map String NamedScratchpads)
    -- ^ Associates the name of a scratchpad to some list of scratchpads
    -- that should be mutually exclusive to it.
  , nspScratchpads :: !(Map String NamedScratchpad)
    -- ^ Associates a name to an entire scratchpad.
  }

instance ExtensionClass NSPState where
  initialValue :: NSPState
  initialValue = NSPState mempty mempty

-- | Try to:
--
--    (i) Fill the 'nspScratchpads' portion of the 'NSPState' with the
--        given list of scratchpads.  In case that particular map of the
--        state is already non-empty, don't do anything and return that
--        state.
--
--   (ii) Replace possibly dummy scratchpads in @nspExclusives@ with
--        proper values.  For convenience, the user may specify
--        exclusive scratchpads by name in the startup hook.  However,
--        we don't necessarily have all information then to immediately
--        turn these into proper NamedScratchpads.  As such, we thinly
--        wrap the names into an NSP skeleton, to be filled in later.
--        This function, to be executed _before_
--        'someNamedScratchpadAction' is the (latest) point where that
--        happens.
fillNSPState :: NamedScratchpads -> X NSPState
fillNSPState nsps = do
    nsp@(NSPState exs scratches) <- XS.get
    if null scratches
      then let nspState = NSPState (fillOut exs) nspScratches
            in nspState <$ XS.put nspState
      else pure nsp
  where
    -- @fillNSPState@ only runs once, so the complexity here is probably
    -- not a big deal.
    nspScratches :: Map String NamedScratchpad
    nspScratches = Map.fromList $ zip (map name nsps) nsps
    fillOut :: Map String [NamedScratchpad] -> Map String [NamedScratchpad]
    fillOut exs = foldl' (\nspMap n -> Map.map (replaceWith n) nspMap) exs nsps
    replaceWith :: NamedScratchpad -> [NamedScratchpad] -> [NamedScratchpad]
    replaceWith n = map (\x -> if name x == name n then n else x)

-- | Manage hook that makes the window non-floating
nonFloating :: ManageHook
nonFloating = idHook

-- | Manage hook that makes the window floating with the default placement
defaultFloating :: ManageHook
defaultFloating = doFloat

-- | Manage hook that makes the window floating with custom placement
customFloating :: W.RationalRect -> ManageHook
customFloating = doRectFloat

-- | @isNSP win nsps@ checks whether the window @win@ is any scratchpad
-- in @nsps@.
isNSP :: Window -> NamedScratchpads -> X Bool
isNSP w = fmap or . traverse ((`runQuery` w) . query)

-- | Named scratchpads configuration
type NamedScratchpads = [NamedScratchpad]

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Runs application which should appear in a specified scratchpad on the workspace it was launched on
runApplicationHere :: NamedScratchpad -> X ()
runApplicationHere = spawnHere . cmd

-- | Action to pop up specified named scratchpad
--
-- Note [Ignored Arguments]: Most of the time, this function ignores its
-- first argument and uses 'NSPState' instead.  The only time when it
-- does not is when no other window has been opened before in the
-- running xmonad instance.  If this is not your use-case, you can
-- safely call this function with an empty list.
namedScratchpadAction :: NamedScratchpads -- ^ Named scratchpads configuration
                      -> String           -- ^ Scratchpad name
                      -> X ()
namedScratchpadAction = customRunNamedScratchpadAction runApplication

-- | Action to pop up specified named scratchpad, initially starting it on the current workspace.
--
-- This function /almost always/ ignores its first argument; see Note
-- [Ignored Arguments] for 'namedScratchpadAction'.
spawnHereNamedScratchpadAction :: NamedScratchpads           -- ^ Named scratchpads configuration
                               -> String                     -- ^ Scratchpad name
                               -> X ()
spawnHereNamedScratchpadAction = customRunNamedScratchpadAction runApplicationHere

-- | Action to pop up specified named scratchpad, given a custom way to initially start the application.
--
-- This function /almost always/ ignores its second argument; see Note
-- [Ignored Arguments] for 'namedScratchpadAction'.
customRunNamedScratchpadAction :: (NamedScratchpad -> X ())  -- ^ Function initially running the application, given the configured @scratchpad@ cmd
                               -> NamedScratchpads           -- ^ Named scratchpads configuration
                               -> String                     -- ^ Scratchpad name
                               -> X ()
customRunNamedScratchpadAction = someNamedScratchpadAction (\f ws -> f $ NE.head ws)

-- | Like 'namedScratchpadAction', but execute the action for all
-- scratchpads that match the query.
--
-- This function /almost always/ ignores its first argument; see Note
-- [Ignored Arguments] for 'namedScratchpadAction'.
allNamedScratchpadAction :: NamedScratchpads
                         -> String
                         -> X ()
allNamedScratchpadAction = someNamedScratchpadAction mapM_ runApplication

-- | A @logHook@ to hide scratchpads when they lose focus.  This can be
-- useful for e.g. dropdown terminals.  Note that this also requires you
-- to use the 'XMonad.Hooks.RefocusLast.refocusLastLogHook'.
--
-- ==== __Example__
--
-- > import XMonad.Hooks.RefocusLast (refocusLastLogHook)
-- > import XMonad.Util.NamedScratchpad
-- >
-- > main = xmonad $ def
-- >   { logHook = refocusLastLogHook
-- >            >> nsHideOnFocusLoss myScratchpads
-- >               -- enable hiding for all of @myScratchpads@
-- >   }
nsHideOnFocusLoss :: NamedScratchpads -> X ()
nsHideOnFocusLoss scratches = withWindowSet $ \winSet -> do
    let cur = W.currentTag winSet
    withRecentsIn cur () $ \lastFocus _ ->
        when (lastFocus `elem` W.index winSet && cur /= scratchpadWorkspaceTag) $
            whenX (isNSP lastFocus scratches) $
                shiftToNSP (W.workspaces winSet) ($ lastFocus)

-- | Execute some action on a named scratchpad.
--
-- This function /almost always/ ignores its third argument; see Note
-- [Ignored Arguments] for 'namedScratchpadAction'.
someNamedScratchpadAction :: ((Window -> X ()) -> NE.NonEmpty Window -> X ())
                          -> (NamedScratchpad -> X ())
                          -> NamedScratchpads
                          -> String
                          -> X ()
someNamedScratchpadAction f runApp _ns scratchpadName = do
    NSPState{ nspScratchpads } <- fillNSPState _ns  -- See Note [Filling NSPState]
    case nspScratchpads !? scratchpadName of
        Just conf -> withWindowSet $ \winSet -> do
            let focusedWspWindows = W.index winSet
                allWindows        = W.allWindows winSet
            matchingOnCurrent <- filterM (runQuery (query conf)) focusedWspWindows
            matchingOnAll     <- filterM (runQuery (query conf)) allWindows

            case NE.nonEmpty matchingOnCurrent of
                -- no matching window on the current workspace -> scratchpad not running or in background
                Nothing -> do
                    -- summon the scratchpad
                    case NE.nonEmpty matchingOnAll of
                        Nothing   -> runApp conf
                        Just wins -> f (windows . W.shiftWin (W.currentTag winSet)) wins
                    -- check for exclusive scratchpads to hide
                    hideUnwanted (name conf)

                -- matching window running on current workspace -> window should be shifted to scratchpad workspace
                Just wins -> shiftToNSP (W.workspaces winSet) (`f` wins)
        Nothing -> return ()

{- Note [Filling NSPState]

We have to potentially populate the state with the given scratchpads
here, in case the manageHook didn't run yet and it's still empty.

For backwards compatibility, 3fc830aa09368dca04df24bf7ec4ac817f2de479
introduced an internal state that's filled in the
namedScratchpadManageHook.  A priori, this means that we would need some
kind of MapRequestEvent to happen before processing scratchpads, since
the manageHook doesn't run otherwise, leaving the extensible state empty
until then.  When trying to open a scratchpad right after starting
xmonad—i.e., before having opened a window—we thus have to populate the
NSPState before looking for scratchpads.

Related: https://github.com/xmonad/xmonad-contrib/issues/728
-}

-- | Tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

-- | Manage hook to use with named scratchpads
namedScratchpadManageHook :: NamedScratchpads -- ^ Named scratchpads configuration
                          -> ManageHook
namedScratchpadManageHook nsps = do
    ns <- Map.elems . nspScratchpads <$> liftX (fillNSPState nsps)
    composeAll $ fmap (\c -> query c --> hook c) ns

-- | Shift some windows to the scratchpad workspace according to the
-- given function.  The workspace is created if necessary.
shiftToNSP :: [WindowSpace] -> ((Window -> X ()) -> X ()) -> X ()
shiftToNSP ws f = do
    unless (any ((scratchpadWorkspaceTag ==) . W.tag) ws) $
        addHiddenWorkspace scratchpadWorkspaceTag
    f (windows . W.shiftWin scratchpadWorkspaceTag)

------------------------------------------------------------------------
-- Dynamic scratchpad functionality

-- $dynamic-scratchpads
--
-- Dynamic scratchpads allow you to declare existing windows as
-- scratchpads.  You can bind a key to make a window start/stop being a
-- scratchpad, and another key to toggle its visibility.  Because
-- dynamic scratchpads are based on existing windows, they have some
-- caveats in comparison to "normal" scratchpads:
--
--   * @xmonad@ has no way of knowing /how/ windows were spawned and
--     thus one is not able to "start" dynamic scratchpads again after
--     the associated window has been closed.
--
--   * If you already have an active dynamic scratchpad @"dyn1"@ and you
--     call 'toggleDynamicNSP' with another window, that window will
--     henceforth occupy the @"dyn1"@ scratchpad.  If you still need the
--     old window, you might have to travel to your scratchpad workspace
--     ('scratchpadWorkspaceTag') in order to retrieve it.
--
-- As an example, the following snippet contains keybindings for two
-- dynamic scratchpads, called @"dyn1"@ and @"dyn2"@:
--
-- > import XMonad.Util.NamedScratchpads
-- >
-- > , ("M-s-a", withFocused $ toggleDynamicNSP "dyn1")
-- > , ("M-s-b", withFocused $ toggleDynamicNSP "dyn2")
-- > , ("M-a"  , dynamicNSPAction "dyn1")
-- > , ("M-b"  , dynamicNSPAction "dyn2")
--

-- | A 'NamedScratchpad' representing a "dynamic" scratchpad; i.e., a
-- scratchpad based on an already existing window.
mkDynamicNSP :: String -> Window -> NamedScratchpad
mkDynamicNSP s w =
    NS { name  = s
       , cmd   = ""               -- we are never going to spawn a dynamic scratchpad
       , query = (w ==) <$> ask
       , hook  = mempty           -- cmd is never called so this will never run
       }

-- | Make a window a dynamic scratchpad
addDynamicNSP :: String -> Window -> X ()
addDynamicNSP s w = XS.modify $ \(NSPState exs ws) ->
  NSPState exs (Map.insert s (mkDynamicNSP s w) ws)

-- | Make a window stop being a dynamic scratchpad
removeDynamicNSP :: String -> X ()
removeDynamicNSP s = XS.modify $ \(NSPState exs ws) -> NSPState exs (Map.delete s ws)

-- | Toggle the visibility of a dynamic scratchpad.
dynamicNSPAction :: String -> X ()
dynamicNSPAction = customRunNamedScratchpadAction (const $ pure ()) []

-- | Either create a dynamic scratchpad out of the given window, or stop
-- a window from being one if it already is.
toggleDynamicNSP :: String -> Window -> X ()
toggleDynamicNSP s w = do
    NSPState{ nspScratchpads } <- XS.get
    case nspScratchpads !? s of
        Nothing  -> addDynamicNSP s w
        Just nsp -> ifM (runQuery (query nsp) w)
                        (removeDynamicNSP s)
                        (addDynamicNSP s w)

-----------------------------------------------------------------------
-- Exclusive scratchpads

-- $exclusive-scratchpads
--
-- Exclusive scratchpads allow you to hide certain scratchpads in
-- relation to others.  There can be multiple groups of pairwise
-- exclusive scratchpads; whenever one such scratchpad gets called, it
-- will hide all other scratchpads on the focused workspace that are in
-- this group.
--
-- For example, having defined "Calc", "Mail", and "Term" scratchpads,
-- you can use 'addExclusives' to make some of them dislike each other:
--
-- > myExclusives = [ ["Calc", "Mail"]
-- >                , ["Mail", "Term"]
-- >                ]
--
-- You now have to add @myExclusives@ to you startupHook:
--
-- > main :: IO
-- > main = xmonad . … . $ def
-- >   { …
-- >   , startupHook = myStartupHook >> myExclusives
-- >   }
--
-- This will hide the "Mail" scratchpad whenever the "Calc" scratchpad
-- is brought up, and vice-versa.  Likewise, "Mail" and "Term" behave in
-- this way, but "Calc" and "Term" may peacefully coexist.
--
-- If you move a scratchpad it still gets hidden when you fetch a
-- scratchpad of the same family.  To change that behaviour—and make
-- windows not exclusive anymore when they get resized or moved—add
-- these mouse bindings (see
-- "XMonad.Doc.Extending#Editing_mouse_bindings"):
--
-- >     , ((mod4Mask, button1), floatMoveNoexclusive)
-- >     , ((mod4Mask, button3), resizeNoexclusive)
--
-- To reset a moved scratchpad to the original position that you set
-- with its hook, focus is and then call 'resetFocusedNSP'.  For
-- example, if you want to extend @M-\<Return\>@ to reset the placement
-- when a scratchpad is in focus but keep the default behaviour for
-- tiled windows, set these key bindings:
--
-- > , ((modMask, xK_Return), windows W.swapMaster >> resetFocusedNSP)

-- | Make some scratchpads exclusive.
addExclusives :: [[String]] -> X ()
addExclusives exs = do
    NSPState _ ws <- XS.get
    -- Re-initialise `ws' to nothing, so we can react to changes in case
    -- of a restart.  See 'fillNSPState' for more details on filling.
    XS.put (NSPState (foldl' (go []) mempty exs) mempty)
    unless (null ws) $
        void (fillNSPState (Map.elems ws))
  where
    -- Ignoring that this is specialised to NSPs, it works something like
    -- >>> foldl' (go []) mempty [[1, 2], [3, 4], [1, 3]]
    -- fromList [(1, [3, 2]), (2, [1]), (3, [1, 4]), (4, [3])]
    go _  m []       = m
    go ms m (n : ns) = go (n : ms) (Map.insertWith (<>) n (mkNSP (ms <> ns)) m) ns
    mkNSP = map (\n -> NS n mempty (pure False) mempty)

-- | @setNoexclusive w@ makes the window @w@ lose its exclusivity
-- features.
setNoexclusive :: Window -> X ()
setNoexclusive w = do
    NSPState _ ws <- XS.get
    whenX (isNSP w (Map.elems ws)) $
        addTag "_NSP_NOEXCLUSIVE" w

-- | If the focused window is a scratchpad, the scratchpad gets reset to
-- the original placement specified with the hook and becomes exclusive
-- again.
resetFocusedNSP :: X ()
resetFocusedNSP = do
    NSPState _ (Map.elems -> ws) <- XS.get
    withFocused $ \w -> do
        mbWin <- findM ((`runQuery` w) . query) ws
        whenJust mbWin $ \win -> do
            (windows . appEndo <=< runQuery (hook win)) w
            hideUnwanted (name win)
            delTag "_NSP_NOEXCLUSIVE" w

-- | @hideUnwanted nspWindow@ hides all windows that @nspWindow@ does
-- not like; i.e., windows that are in some kind of exclusivity contract
-- with it.
--
-- A consistency assumption for this is that @nspWindow@ must be the
-- currently focused window.  For this to take effect, @nspWindow@ must
-- not have set the @_NSP_NOEXCLUSIVE@ property, neither must any
-- exclusive window we'd like to hide.
hideUnwanted :: String -> X ()
hideUnwanted nspWindow = withWindowSet $ \winSet -> do
    NSPState{ nspExclusives } <- XS.get
    whenJust (nspExclusives !? nspWindow) $ \unwanted ->
        withFocused $ \w -> whenX (runQuery notIgnored w) $ do
            for_ (W.index winSet) $ \win ->
                whenX (runQuery (isUnwanted unwanted) win) $
                    shiftToNSP (W.workspaces winSet) ($ win)
  where
    notIgnored :: Query Bool
    notIgnored = notElem "_NSP_NOEXCLUSIVE" . words <$> stringProperty "_XMONAD_TAGS"

    isUnwanted :: [NamedScratchpad] -> Query Bool
    isUnwanted = (notIgnored <&&>) . foldr (\nsp qs -> qs <||> query nsp) (pure False)

-- | Float and drag the window; make it lose its exclusivity status in
-- the process.
floatMoveNoexclusive :: Window -- ^ Window which should be moved
                     -> X ()
floatMoveNoexclusive = mouseHelper mouseMoveWindow

-- | Resize window and make it lose its exclusivity status in the
-- process.
resizeNoexclusive :: Window -- ^ Window which should be resized
                  -> X ()
resizeNoexclusive = mouseHelper mouseResizeWindow

mouseHelper :: (Window -> X a) -> Window -> X ()
mouseHelper f w = setNoexclusive w
               >> focus w
               >> f w
               >> windows W.shiftMaster

------------------------------------------------------------------------
-- Deprecations

-- | Transforms a workspace list containing the NSP workspace into one that
-- doesn't contain it. Intended for use with logHooks.
namedScratchpadFilterOutWorkspace :: [WindowSpace] -> [WindowSpace]
namedScratchpadFilterOutWorkspace = filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)
{-# DEPRECATED namedScratchpadFilterOutWorkspace "Use XMonad.Util.WorkspaceCompare.filterOutWs [scratchpadWorkspaceTag] instead" #-}

-- | Transforms a pretty-printer into one not displaying the NSP workspace.
--
-- A simple use could be:
--
-- > logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspace $ def
--
-- Here is another example, when using "XMonad.Layout.IndependentScreens".
-- If you have handles @hLeft@ and @hRight@ for bars on the left and right screens, respectively, and @pp@ is a pretty-printer function that takes a handle, you could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
namedScratchpadFilterOutWorkspacePP :: PP -> PP
namedScratchpadFilterOutWorkspacePP pp = pp {
  ppSort = fmap (. namedScratchpadFilterOutWorkspace) (ppSort pp)
  }
{-# DEPRECATED namedScratchpadFilterOutWorkspacePP "Use XMonad.Hooks.StatusBar.PP.filterOutWsPP [scratchpadWorkspaceTag] instead" #-}
