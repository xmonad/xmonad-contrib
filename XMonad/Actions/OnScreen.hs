-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.OnScreen
-- Description :  Control workspaces on different screens (in xinerama mode).
-- Copyright   :  (c) 2009 Nils Schweinsberg
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Control workspaces on different screens (in xinerama mode).
--
-----------------------------------------------------------------------------

module XMonad.Actions.OnScreen (
    -- * Usage
    -- $usage
      onScreen
    , onScreen'
    , Focus(..)
    , viewOnScreen
    , greedyViewOnScreen
    , onlyOnScreen
    , toggleOnScreen
    , toggleGreedyOnScreen
    ) where

import XMonad
import XMonad.Prelude (fromMaybe, guard)
import XMonad.StackSet hiding (new)


-- | Focus data definitions
data Focus = FocusNew                       -- ^ always focus the new screen
           | FocusCurrent                   -- ^ always keep the focus on the current screen
           | FocusTag WorkspaceId           -- ^ always focus tag i on the new stack
           | FocusTagVisible WorkspaceId    -- ^ focus tag i only if workspace with tag i is visible on the old stack


-- | Run any function that modifies the stack on a given screen. This function
-- will also need to know which Screen to focus after the function has been
-- run.
onScreen :: (WindowSet -> WindowSet) -- ^ function to run
         -> Focus                    -- ^ what to do with the focus
         -> ScreenId                 -- ^ screen id
         -> WindowSet                -- ^ current stack
         -> WindowSet
onScreen f foc sc st = fromMaybe st $ do
    ws <- lookupWorkspace sc st

    let fStack      = f $ view ws st

    return $ setFocus foc st fStack


-- set focus for new stack
setFocus :: Focus
         -> WindowSet -- ^ old stack
         -> WindowSet -- ^ new stack
         -> WindowSet
setFocus FocusNew _ new             = new
setFocus FocusCurrent old new        =
    case lookupWorkspace (screen $ current old) new of
         Nothing -> new
         Just i -> view i new
setFocus (FocusTag i) _ new         = view i new
setFocus (FocusTagVisible i) old new =
    if i `elem` map (tag . workspace) (visible old)
       then setFocus (FocusTag i) old new
       else setFocus FocusCurrent old new

-- | A variation of @onScreen@ which will take any @X ()@ function and run it
-- on the given screen.
-- Warning: This function will change focus even if the function it's supposed
-- to run doesn't succeed.
onScreen' :: X ()       -- ^ X function to run
          -> Focus      -- ^ focus
          -> ScreenId   -- ^ screen id
          -> X ()
onScreen' x foc sc = do
    st <- gets windowset
    case lookupWorkspace sc st of
         Nothing -> return ()
         Just ws -> do
             windows $ view ws
             x
             windows $ setFocus foc st


-- | Switch to workspace @i@ on screen @sc@. If @i@ is visible use @view@ to
-- switch focus to the workspace @i@.
viewOnScreen :: ScreenId    -- ^ screen id
             -> WorkspaceId -- ^ index of the workspace
             -> WindowSet   -- ^ current stack
             -> WindowSet
viewOnScreen sid i =
    onScreen (view i) (FocusTag i) sid

-- | Switch to workspace @i@ on screen @sc@. If @i@ is visible use @greedyView@
-- to switch the current workspace with workspace @i@.
greedyViewOnScreen :: ScreenId    -- ^ screen id
                   -> WorkspaceId -- ^ index of the workspace
                   -> WindowSet   -- ^ current stack
                   -> WindowSet
greedyViewOnScreen sid i =
    onScreen (greedyView i) (FocusTagVisible i) sid

-- | Switch to workspace @i@ on screen @sc@. If @i@ is visible do nothing.
onlyOnScreen :: ScreenId    -- ^ screen id
             -> WorkspaceId -- ^ index of the workspace
             -> WindowSet   -- ^ current stack
             -> WindowSet
onlyOnScreen sid i =
    onScreen (view i) FocusCurrent sid

-- | @toggleOrView@ as in "XMonad.Actions.CycleWS" for @onScreen@ with view
toggleOnScreen :: ScreenId    -- ^ screen id
               -> WorkspaceId -- ^ index of the workspace
               -> WindowSet   -- ^ current stack
               -> WindowSet
toggleOnScreen sid i =
    onScreen (toggleOrView' view i) FocusCurrent sid

-- | @toggleOrView@ from "XMonad.Actions.CycleWS" for @onScreen@ with greedyView
toggleGreedyOnScreen :: ScreenId    -- ^ screen id
                     -> WorkspaceId -- ^ index of the workspace
                     -> WindowSet   -- ^ current stack
                     -> WindowSet
toggleGreedyOnScreen sid i =
    onScreen (toggleOrView' greedyView i) FocusCurrent sid


-- a \"pure\" version of X.A.CycleWS.toggleOrDoSkip
toggleOrView' :: (WorkspaceId -> WindowSet -> WindowSet)   -- ^ function to run
              -> WorkspaceId                               -- ^ tag to look for
              -> WindowSet                                 -- ^ current stackset
              -> WindowSet
toggleOrView' f i st = fromMaybe (f i st) $ do
    let st' = hidden st
    -- make sure we actually have to do something
    guard $ i == (tag . workspace $ current st)
    guard $ not (null st')
    -- finally, toggle!
    return $ f (tag . head $ st') st


-- $usage
--
-- This module provides an easy way to control, what you see on other screens in
-- xinerama mode without having to focus them. Put this into your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.OnScreen
--
-- Then add the appropriate keybindings, for example replace your current keys
-- to switch the workspaces with this at the bottom of your keybindings:
--
-- >     ++
-- >     [ ((m .|. modm, k), windows (f i))
-- >       | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
-- >       , (f, m) <- [ (viewOnScreen 0, 0)
-- >                   , (viewOnScreen 1, controlMask)
-- >                   , (greedyView, controlMask .|. shiftMask) ]
-- >     ]
--
-- This will provide you with the following keybindings:
--
--      * modkey + 1-0:
--      Switch to workspace 1-0 on screen 0
--
--      * modkey + control + 1-0:
--      Switch to workspace 1-0 on screen 1
--
--      * modkey + control + shift + 1-0:
--      Default greedyView behaviour
--
--
-- A more basic version inside the default keybindings would be:
--
-- >        , ((modm .|. controlMask, xK_1), windows (viewOnScreen 0 "1"))
--
-- where 0 is the first screen and \"1\" the workspace with the tag \"1\".
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
