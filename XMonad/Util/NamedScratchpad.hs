{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
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

  -- * Deprecations
  namedScratchpadFilterOutWorkspace,
  namedScratchpadFilterOutWorkspacePP,

  ) where

import Data.Coerce (coerce)
import Data.Map.Strict (Map, (!?))
import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.RefocusLast (withRecentsIn)
import XMonad.Hooks.StatusBar.PP (PP, ppSort)
import XMonad.Prelude (filterM, unless, when)

import qualified Data.Map.Strict    as Map
import qualified Data.List.NonEmpty as NE

import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS

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

-- | The NSP state associates a name to an entire scratchpad.
newtype NSPState = NSPState (Map String NamedScratchpad)

instance ExtensionClass NSPState where
  initialValue :: NSPState
  initialValue = NSPState mempty

-- | Construct an 'NSPState' from an ordinary list of scratchpads.
buildNSPState :: NamedScratchpads -> NSPState
buildNSPState nsps = NSPState . Map.fromList $ zip (map name nsps) nsps

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

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Runs application which should appear in a specified scratchpad on the workspace it was launched on
runApplicationHere :: NamedScratchpad -> X ()
runApplicationHere = spawnHere . cmd

-- | Action to pop up specified named scratchpad
--
-- NOTE: Ignores its first argument and uses 'NSPState' instead.
namedScratchpadAction :: NamedScratchpads -- ^ Named scratchpads configuration
                      -> String           -- ^ Scratchpad name
                      -> X ()
namedScratchpadAction = customRunNamedScratchpadAction runApplication

-- | Action to pop up specified named scratchpad, initially starting it on the current workspace.
--
-- NOTE: Ignores its first argument and uses 'NSPState' instead.
spawnHereNamedScratchpadAction :: NamedScratchpads           -- ^ Named scratchpads configuration
                               -> String                     -- ^ Scratchpad name
                               -> X ()
spawnHereNamedScratchpadAction = customRunNamedScratchpadAction runApplicationHere

-- | Action to pop up specified named scratchpad, given a custom way to initially start the application.
--
-- NOTE: Ignores its second argument and uses 'NSPState' instead.
customRunNamedScratchpadAction :: (NamedScratchpad -> X ())  -- ^ Function initially running the application, given the configured @scratchpad@ cmd
                               -> NamedScratchpads           -- ^ Named scratchpads configuration
                               -> String                     -- ^ Scratchpad name
                               -> X ()
customRunNamedScratchpadAction = someNamedScratchpadAction (\f ws -> f $ NE.head ws)

-- | Like 'namedScratchpadAction', but execute the action for all
-- scratchpads that match the query.
--
-- NOTE: Ignores its first argument and uses 'NSPState' instead.
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
            whenX (isNS lastFocus) $
                shiftToNSP (W.workspaces winSet) ($ lastFocus)
  where
    isNS :: Window -> X Bool
    isNS w = or <$> traverse ((`runQuery` w) . query) scratches

-- | Execute some action on a named scratchpad.
--
-- NOTE: Ignores its first argument and uses 'NSPState' instead.
someNamedScratchpadAction :: ((Window -> X ()) -> NE.NonEmpty Window -> X ())
                          -> (NamedScratchpad -> X ())
                          -> NamedScratchpads
                          -> String
                          -> X ()
someNamedScratchpadAction f runApp _ns scratchpadName = do
    NSPState scratchpadConfig <- XS.get
    case scratchpadConfig !? scratchpadName of
        Just conf -> withWindowSet $ \winSet -> do
            let focusedWspWindows = maybe [] W.integrate (W.stack . W.workspace . W.current $ winSet)
                allWindows        = W.allWindows winSet
            matchingOnCurrent <- filterM (runQuery (query conf)) focusedWspWindows
            matchingOnAll     <- filterM (runQuery (query conf)) allWindows

            case NE.nonEmpty matchingOnCurrent of
                -- no matching window on the current workspace -> scratchpad not running or in background
                Nothing -> case NE.nonEmpty matchingOnAll of
                    Nothing   -> runApp conf
                    Just wins -> f (windows . W.shiftWin (W.currentTag winSet)) wins

                -- matching window running on current workspace -> window should be shifted to scratchpad workspace
                Just wins -> shiftToNSP (W.workspaces winSet) (`f` wins)
        Nothing -> return ()

-- | Tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

-- | Manage hook to use with named scratchpads
namedScratchpadManageHook :: NamedScratchpads -- ^ Named scratchpads configuration
                          -> ManageHook
namedScratchpadManageHook nsps = do
    ns <- liftX $ XS.get >>= \(NSPState xs) ->
        if   null xs
        then nsps <$ XS.put (buildNSPState nsps)
        else pure $ Map.elems xs
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
addDynamicNSP s w = XS.modify @NSPState . coerce $ Map.insert s (mkDynamicNSP s w)

-- | Make a window stop being a dynamic scratchpad
removeDynamicNSP :: String -> X ()
removeDynamicNSP s = XS.modify @NSPState . coerce $ Map.delete @_ @NamedScratchpad s

-- | Toggle the visibility of a dynamic scratchpad.
dynamicNSPAction :: String -> X ()
dynamicNSPAction = customRunNamedScratchpadAction (const $ pure ()) []

-- | Either create a dynamic scratchpad out of the given window, or stop
-- a window from being one if it already is.
toggleDynamicNSP :: String -> Window -> X ()
toggleDynamicNSP s w = do
    NSPState nsps <- XS.get
    case nsps !? s of
        Nothing  -> addDynamicNSP s w
        Just nsp -> ifM (runQuery (query nsp) w)
                        (removeDynamicNSP s)
                        (addDynamicNSP s w)

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
