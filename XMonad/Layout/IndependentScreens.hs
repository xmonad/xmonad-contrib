{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IndependentScreens
-- Description :  Simulate independent sets of workspaces on each screen (dwm-like).
-- Copyright   :  (c) 2009 Daniel Wagner
-- License     :  BSD3
--
-- Maintainer  :  <daniel@wagner-home.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utility functions for simulating independent sets of workspaces on
-- each screen (like dwm's workspace model), using internal tags to
-- distinguish workspaces associated with each screen.
-----------------------------------------------------------------------------

module XMonad.Layout.IndependentScreens (
    -- * Usage
    -- $usage
    VirtualWorkspace, PhysicalWorkspace,
    VirtualWindowSpace, PhysicalWindowSpace,
    workspaces',
    withScreen, withScreens,
    onCurrentScreen,
    marshallPP,
    whenCurrentOn,
    countScreens,
    workspacesOn,
    workspaceOnScreen, focusWindow', focusScreen, nthWorkspace, withWspOnScreen,
    -- * Converting between virtual and physical workspaces
    -- $converting
    marshall, unmarshall, unmarshallS, unmarshallW,
    marshallWindowSpace, unmarshallWindowSpace, marshallSort,
) where

import Control.Arrow ((***))
import Graphics.X11.Xinerama
import XMonad
import XMonad.Hooks.StatusBar.PP
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.IndependentScreens
--
-- You can define your workspaces by calling @withScreens@:
--
-- > myConfig = def { workspaces = withScreens 2 ["web", "email", "irc"] }
--
-- This will create \"physical\" workspaces with distinct internal names for
-- each (screen, virtual workspace) pair.
--
-- Then edit any keybindings that use the list of workspaces or refer
-- to specific workspace names.  In the default configuration, only
-- the keybindings for changing workspace do this:
--
-- > keyBindings conf = let modm = modMask conf in fromList $
-- >     {- lots of other keybindings -}
-- >     [((m .|. modm, k), windows $ f i)
-- >         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
-- >         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--
-- This should change to
--
-- > keyBindings conf = let modm = modMask conf in fromList $
-- >     {- lots of other keybindings -}
-- >     [((m .|. modm, k), windows $ onCurrentScreen f i)
-- >         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
-- >         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--
-- In particular, the analogue of @XMonad.workspaces@ is
-- @workspaces'@, and you can use @onCurrentScreen@ to convert functions
-- of virtual workspaces to functions of physical workspaces, which work
-- by marshalling the virtual workspace name and the currently focused
-- screen into a physical workspace name.
--
-- A complete example abusing many of the functions below is available in the
-- <https://xmonad.org/configurations.html XMonad.Config.Dmwit> configuration.

type VirtualWorkspace  = WorkspaceId
type PhysicalWorkspace = WorkspaceId

-- | A 'WindowSpace' whose tags are 'PhysicalWorkspace's.
type PhysicalWindowSpace = WindowSpace
-- | A 'WindowSpace' whose tags are 'VirtualWorkspace's.
type VirtualWindowSpace = WindowSpace

-- $converting
-- You shouldn't need to use the functions below very much. They are used
-- internally. However, in some cases, they may be useful, and so are exported
-- just in case. In general, the \"marshall\" functions convert the convenient
-- form (like \"web\") you would like to use in your configuration file to the
-- inconvenient form (like \"2_web\") that xmonad uses internally. Similarly,
-- the \"unmarshall\" functions convert in the other direction.

marshall :: ScreenId -> VirtualWorkspace -> PhysicalWorkspace
marshall (S sc) vws = show sc ++ '_':vws

unmarshall  :: PhysicalWorkspace -> (ScreenId, VirtualWorkspace)
unmarshallS :: PhysicalWorkspace -> ScreenId
unmarshallW :: PhysicalWorkspace -> VirtualWorkspace

unmarshall  = ((S . read) *** drop 1) . break (=='_')
unmarshallS = fst . unmarshall
unmarshallW = snd . unmarshall

-- | Get a list of all the virtual workspace names.
workspaces' :: XConfig l -> [VirtualWorkspace]
workspaces' = nub . map unmarshallW . workspaces

-- | Specify workspace names for each screen
withScreen :: ScreenId            -- ^ The screen to make workspaces for
           -> [VirtualWorkspace]  -- ^ The desired virtual workspace names
           -> [PhysicalWorkspace] -- ^ A list of all internal physical workspace names
withScreen n = map (marshall n)

-- | Make all workspaces across the monitors bear the same names
withScreens :: ScreenId            -- ^ The number of screens to make workspaces for
            -> [VirtualWorkspace]  -- ^ The desired virtual workspace names
            -> [PhysicalWorkspace] -- ^ A list of all internal physical workspace names
withScreens n vws = concatMap (`withScreen` vws) [0..n-1]

-- | Transform a function over physical workspaces into a function over virtual workspaces.
-- This is useful as it allows you to write code without caring about the current screen, i.e. to say "switch to workspace 3"
-- rather than saying "switch to workspace 3 on monitor 3".
onCurrentScreen :: (PhysicalWorkspace -> WindowSet -> a) -> (VirtualWorkspace -> WindowSet -> a)
onCurrentScreen f vws ws =
  let currentScreenId = W.screen $ W.current ws
   in f (marshall currentScreenId vws) ws

-- | Get the workspace currently active on a given screen
workspaceOnScreen :: ScreenId -> WindowSet -> Maybe PhysicalWorkspace
workspaceOnScreen screenId ws = W.tag . W.workspace <$> screenOnMonitor screenId ws

-- | Generate WindowSet transformation by providing a given function with the workspace active on a given screen.
-- This may for example be used to shift a window to another screen as follows:
--
-- > windows $ withWspOnScreen 1 W.shift
--
withWspOnScreen :: ScreenId                               -- ^ The screen to run on
         -> (PhysicalWorkspace -> WindowSet -> WindowSet) -- ^ The transformation that will be passed the workspace currently active on there
         -> WindowSet -> WindowSet
withWspOnScreen screenId operation ws = case workspaceOnScreen screenId ws of
    Just wsp -> operation wsp ws
    Nothing -> ws

-- | Get the workspace that is active on a given screen.
screenOnMonitor :: ScreenId -> WindowSet -> Maybe WindowScreen
screenOnMonitor screenId ws = find ((screenId ==) . W.screen) (W.current ws : W.visible ws)

-- | Focus a window, switching workspace on the correct Xinerama screen if neccessary.
focusWindow' :: Window -> WindowSet -> WindowSet
focusWindow' window ws
  | Just window == W.peek ws = ws
  | otherwise = case W.findTag window ws of
      Just tag -> W.focusWindow window $ focusScreen (unmarshallS tag) ws
      Nothing -> ws

-- | Focus a given screen.
focusScreen :: ScreenId -> WindowSet -> WindowSet
focusScreen screenId = withWspOnScreen screenId W.view

-- | Get the nth virtual workspace
nthWorkspace :: Int -> X (Maybe VirtualWorkspace)
nthWorkspace n = (!? n) . workspaces' <$> asks config

-- | In case you don't know statically how many screens there will be, you can call this in main before starting xmonad.  For example, part of my config reads
--
-- > main = do
-- >   nScreens <- countScreens
-- >   xmonad $ def {
-- >     ...
-- >     workspaces = withScreens nScreens (workspaces def),
-- >     ...
-- >     }
--
countScreens :: (MonadIO m, Integral i) => m i
countScreens = fmap genericLength . liftIO $ openDisplay "" >>= liftA2 (<*) getScreenInfo closeDisplay

-- | This turns a pretty-printer into one that is aware of the independent screens. The
-- converted pretty-printer first filters out physical workspaces on other screens, then
-- converts all the physical workspaces on this screen to their virtual names.
-- Note that 'ppSort' still operates on physical (marshalled) workspace names,
-- otherwise functions from "XMonad.Util.WorkspaceCompare" wouldn't work.
-- If you need to sort on virtual names, see 'marshallSort'.
--
-- For example, if you have have two bars on the left and right screens, respectively, and @pp@ is
-- a pretty-printer, you could apply 'marshallPP' when creating a @StatusBarConfig@ from "XMonad.Hooks.StatusBar".
--
-- A sample config looks like this:
--
-- > mySBL = statusBarProp "xmobar" $ pure (marshallPP (S 0) pp)
-- > mySBR = statusBarProp "xmobar" $ pure (marshallPP (S 1) pp)
-- > main = xmonad $ withEasySB (mySBL <> mySBR) defToggleStrutsKey def
--
marshallPP :: ScreenId -> PP -> PP
marshallPP s pp = pp { ppRename = ppRename pp . unmarshallW
                     , ppSort = (. workspacesOn s) <$> ppSort pp }

-- | Take a pretty-printer and turn it into one that only runs when the current
-- workspace is one associated with the given screen. The way this works is a
-- bit hacky, so beware: the 'ppOutput' field of the input will not be invoked
-- if either of the following conditions is met:
--
-- 1. The 'ppSort' of the input returns an empty list (when not given one).
--
-- 2. The 'ppOrder' of the input returns the exact string @\"\\0\"@.
--
-- For example, you can use this to create a pipe which tracks the title of the
-- window currently focused on a given screen (even if the screen is not
-- current) by doing something like this:
--
-- > ppFocus s = whenCurrentOn s def
-- >     { ppOrder  = \(_:_:title:_) -> [title]
-- >     , ppOutput = appendFile ("focus" ++ show s) . (++ "\n")
-- >     }
--
-- Sequence a few of these pretty-printers to get a log hook that keeps each
-- screen's title up-to-date.
whenCurrentOn :: ScreenId -> PP -> PP
whenCurrentOn s pp = pp
    { ppSort = do
        sorter <- ppSort pp
        pure $ \case xs@(x:_) | unmarshallS (W.tag x) == s -> sorter xs
                     _ -> []

    , ppOrder  = \case ("":_) -> ["\0"] -- we got passed no workspaces; this is the signal from ppSort that this is a boring case
                       list   -> ppOrder pp list

    , ppOutput = \case "\0"   -> pure () -- we got passed the signal from ppOrder that this is a boring case
                       output -> ppOutput pp output
    }

-- | Filter workspaces that are on a given screen.
workspacesOn :: ScreenId -> [PhysicalWindowSpace] -> [PhysicalWindowSpace]
workspacesOn s = filter (\ws -> unmarshallS (W.tag ws) == s)

-- | @vSort@ is a function that sorts 'VirtualWindowSpace's with virtual names.
-- @marshallSort s vSort@ is a function which sorts 'PhysicalWindowSpace's with virtual names,
-- but keeps only the 'WindowSpace'\'s on screen @s@.
--
-- NOTE: @vSort@ operating on virtual names comes with some caveats, see
-- <https://github.com/xmonad/xmonad-contrib/issues/420 this issue> for
-- more information. You can use 'marshallSort' like in the following example:
--
-- === __Example__
--
-- > pp' :: ScreenId -> PP -> PP
-- > pp' s pp = (marshallPP s pp) { ppSort = fmap (marshallSort s) (ppSort pp) }
-- >
-- > mySBL = statusBarProp "xmobar" $ pure (pp' (S 0) pp)
-- > mySBR = statusBarProp "xmobar" $ pure (pp' (S 1) pp)
-- > main = xmonad $ withEasySB (mySBL <> mySBR) defToggleStrutsKey def
--
-- In this way, you have a custom virtual names sort on top of 'marshallPP'.
marshallSort :: ScreenId -> ([VirtualWindowSpace] -> [VirtualWindowSpace]) -> ([PhysicalWindowSpace] -> [PhysicalWindowSpace])
marshallSort s vSort = pScreens . vSort . vScreens where
    vScreens    = map unmarshallWindowSpace . workspacesOn s
    pScreens    = map (marshallWindowSpace s)

-- | Convert the tag of the 'WindowSpace' from a 'VirtualWorkspace' to a 'PhysicalWorkspace'.
marshallWindowSpace   :: ScreenId -> WindowSpace -> WindowSpace
-- | Convert the tag of the 'WindowSpace' from a 'PhysicalWorkspace' to a 'VirtualWorkspace'.
unmarshallWindowSpace :: WindowSpace -> WindowSpace

marshallWindowSpace s ws = ws { W.tag = marshall s  (W.tag ws) }
unmarshallWindowSpace ws = ws { W.tag = unmarshallW (W.tag ws) }
