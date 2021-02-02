{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IndependentScreens
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
    workspaces',
    withScreens, onCurrentScreen,
    marshallPP,
    whenCurrentOn,
    countScreens,
    -- * Converting between virtual and physical workspaces
    -- $converting
    marshall, unmarshall, unmarshallS, unmarshallW,
    marshallWindowSpace, unmarshallWindowSpace, marshallSort,
    workspaceOnScreen, focusWindow', focusScreen, nthWorkspace, withWspOnScreen, activateWindowEventHook
   ) where

-- for the screen stuff
import Control.Applicative(liftA2)
import Control.Arrow hiding ((|||))
import Data.List (find, nub, genericLength)
import Graphics.X11.Xinerama
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import Data.Monoid (All(All))
import Control.Monad (when)

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
-- "XMonad.Config.Dmwit" module.

type VirtualWorkspace  = WorkspaceId
type PhysicalWorkspace = WorkspaceId

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
workspaces' = nub . map (snd . unmarshall) . workspaces

withScreens :: ScreenId            -- ^ The number of screens to make workspaces for
            -> [VirtualWorkspace]  -- ^ The desired virtual workspace names
            -> [PhysicalWorkspace] -- ^ A list of all internal physical workspace names
withScreens n vws = [marshall sc pws | pws <- vws, sc <- [0..n-1]]

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


-- | generate WindowSet transformation by providing a given function with the workspace active on a given screen.
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
screenOnMonitor :: ScreenId -> WindowSet -> Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
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
nthWorkspace n = (!!? n) . workspaces' <$> asks config


-- | HandleEventHook which makes the activate window event respect IndependentScreens.
-- Without this, a window requesting activation may cause a workspace switch
-- that shows a workspace from a different screen on the current one.
activateWindowEventHook :: Event -> X All
activateWindowEventHook ClientMessageEvent { ev_message_type = messageType, ev_window = window } = do
  activateWindowAtom <- getAtom "_NET_ACTIVE_WINDOW"
  when (messageType == activateWindowAtom) $
    windows (focusWindow' window)
  return $ All True
activateWindowEventHook _ = return $ All True


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

-- | This turns a naive pretty-printer into one that is aware of the
-- independent screens. That is, you can write your pretty printer to behave
-- the way you want on virtual workspaces; this function will convert that
-- pretty-printer into one that first filters out physical workspaces on other
-- screens, then converts all the physical workspaces on this screen to their
-- virtual names.
--
-- For example, if you have handles @hLeft@ and @hRight@ for bars on the left and right screens, respectively, and @pp@ is a pretty-printer function that takes a handle, you could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
marshallPP :: ScreenId -> PP -> PP
marshallPP s pp = pp {
    ppCurrent           = ppCurrent         pp . snd . unmarshall,
    ppVisible           = ppVisible         pp . snd . unmarshall,
    ppHidden            = ppHidden          pp . snd . unmarshall,
    ppHiddenNoWindows   = ppHiddenNoWindows pp . snd . unmarshall,
    ppUrgent            = ppUrgent          pp . snd . unmarshall,
    ppSort              = fmap (marshallSort s) (ppSort pp)
    }

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
        sort <- ppSort pp
        pure $ \xs -> case xs of
                        x:_ | unmarshallS (W.tag x) == s -> sort xs
                        _                                -> []
    , ppOrder  = \case ("":_) -> ["\0"] -- we got passed no workspaces; this is the signal from ppSort that this is a boring case
                       list   -> ppOrder pp list

    , ppOutput = \case "\0"   -> pure () -- we got passed the signal from ppOrder that this is a boring case
                       output -> ppOutput pp output
    }

-- | If @vSort@ is a function that sorts 'WindowSpace's with virtual names, then @marshallSort s vSort@ is a function which sorts 'WindowSpace's with physical names in an analogous way -- but keeps only the spaces on screen @s@.
marshallSort :: ScreenId -> ([WindowSpace] -> [WindowSpace]) -> ([WindowSpace] -> [WindowSpace])
marshallSort s vSort = pScreens . vSort . vScreens where
    isOnScreen ws = unmarshallS (W.tag ws) == s
    vScreens      = map unmarshallWindowSpace . filter isOnScreen
    pScreens      = map (marshallWindowSpace s)

-- | Convert the tag of the 'WindowSpace' from a 'VirtualWorkspace' to a 'PhysicalWorkspace'.
marshallWindowSpace   :: ScreenId -> WindowSpace -> WindowSpace
-- | Convert the tag of the 'WindowSpace' from a 'PhysicalWorkspace' to a 'VirtualWorkspace'.
unmarshallWindowSpace :: WindowSpace -> WindowSpace

marshallWindowSpace s ws = ws { W.tag = marshall s  (W.tag ws) }
unmarshallWindowSpace ws = ws { W.tag = unmarshallW (W.tag ws) }

-- | Safe version of (!!)
(!!?) :: [a] -> Int -> Maybe a
(!!?) list n
  | n < length list = Just $ list !! n
  | otherwise = Nothing
