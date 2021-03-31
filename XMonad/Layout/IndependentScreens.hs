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
    withScreen, withScreens,
    onCurrentScreen,
    marshallPP, marshallPP',
    whenCurrentOn,
    countScreens,
    workspacesOn,
    -- * Converting between virtual and physical workspaces
    -- $converting
    marshall, unmarshall, unmarshallS, unmarshallW,
    marshallWindowSpace, unmarshallWindowSpace, marshallSort, marshallSort',
    -- * "XMonad.Actions.CopyWindow" integration
    -- $integration
    wsContainingCopies,
    copyWindowTo,
    -- * "XMonad.Actions.DynamicWorkspaces" integration
    -- $integration2
    withNthWorkspaceScreen
) where

-- for the screen stuff
import Control.Applicative(liftA2)
import Control.Arrow hiding ((|||))
import Data.List (nub, genericLength)
import Graphics.X11.Xinerama
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CopyWindow (copy, copiesOfOn, taggedWindows)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

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

workspaces' :: XConfig l -> [VirtualWorkspace]
workspaces' = nub . map unmarshallW . workspaces

-- | Workspace names are independently specified with each monitor.
-- You can define your workspaces by calling withScreen for each screen:
--
-- > myConfig = def { workspaces = withScreen 0 ["web", "email", "irc"]  ++ withScreen 1 ["1", "2", "3"] }
withScreen :: ScreenId            -- ^ The screen to make workspaces for
           -> [VirtualWorkspace]  -- ^ The desired virtual workspace names
           -> [PhysicalWorkspace] -- ^ A list of all internal physical workspace names
withScreen n vws = [marshall n pws | pws <- vws]

-- | Make all workspaces across the monitors bear the same names
withScreens :: ScreenId            -- ^ The number of screens to make workspaces for
            -> [VirtualWorkspace]  -- ^ The desired virtual workspace names
            -> [PhysicalWorkspace] -- ^ A list of all internal physical workspace names
withScreens n vws = concatMap (`withScreen` vws) [0..n-1]

onCurrentScreen :: (VirtualWorkspace -> WindowSet -> a) -> (PhysicalWorkspace -> WindowSet -> a)
onCurrentScreen f vws = W.screen . W.current >>= f . flip marshall vws

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

-- | Copy the focused window to a workspace on the current screen;
-- replacement for 'XMonad.Actions.CopyWindow.copy'.
--
-- Key binding example:
--
-- > ((modm, xK_v ), copyWindowTo 1) -- copy focused window to the 1st workspace on current screen.
copyWindowTo :: Int -> X ()
copyWindowTo j = do
    winset <- gets windowset
    sort <- getSortByIndex
    let ws = sort . W.workspaces $ winset
        sc = W.screen . W.current $ winset
        wsID = [ W.tag m | m <- ws, unmarshallS (W.tag m) == sc]
    windows $ copy $ wsID !! (j - 1)

-- | A list of hidden workspaces containing a copy of the focused window on the current screen.
wsContainingCopies :: X [WorkspaceId]
wsContainingCopies = do
    ws <- gets windowset
    let sc = W.screen . W.current $ ws
    return $ copiesOfOn (W.peek ws) (taggedWindows $ workspaceHidden ws sc)
  where
    workspaceHidden ws sc = [ w | w <- W.hidden ws, sc == unmarshallS (W.tag w) ]

-- | Do something on the current screen with the nth workspace in the
-- dynamic order. The callback is given the workspace's tag as well as
-- the 'WindowSet' of the workspace itself.
--
-- Replacement for 'XMonad.Actions.DynamicWorkspaces.withNthWorkspace'.
withNthWorkspaceScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspaceScreen job wnum = do
    winset <- gets windowset
    sort <- getSortByIndex
    let ws = sort . W.workspaces $ winset
        sc = W.screen . W.current $ winset
        enoughWorkspaces = drop wnum [W.tag j | j <- ws, unmarshallS (W.tag j) == sc]
    case enoughWorkspaces of
        (w : _) -> windows $ job w
        []      -> return ()

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
marshallPP s pp = pp { ppRename = ppRename pp . unmarshallW
                     , ppSort = fmap (marshallSort s) (ppSort pp) }

-- | Like 'marshallPP', but uses 'marshallSort\'' instead of 'marshallSort'.
marshallPP' :: ScreenId -> PP -> PP
marshallPP' s pp = (marshallPP s pp){ ppSort = fmap (marshallSort' s) (ppSort pp) }

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
        return $ \xs -> case xs of
            x:_ | unmarshallS (W.tag x) == s -> sort xs
            _ -> []
    , ppOrder = \i@(wss:_) -> case wss of
        "" -> ["\0"] -- we got passed no workspaces; this is the signal from ppSort that this is a boring case
        _  -> ppOrder pp i
    , ppOutput = \out -> case out of
        "\0" -> return () -- we got passed the signal from ppOrder that this is a boring case
        _ -> ppOutput pp out
    }

-- | Filter workspaces that are on current screen.
workspacesOn :: ScreenId -> [PhysicalWindowSpace] -> [PhysicalWindowSpace]
workspacesOn s = filter (\ws -> unmarshallS (W.tag ws) == s)

-- | @vSort@ is a function that sorts 'VirtualWindowSpace's with virtual names.
-- @marshallSort s vSort@ is a function which sorts 'PhysicalWindowSpace's with virtual names, 
-- but keeps only the 'WindowSpace'\'s on screen @s@.
marshallSort :: ScreenId -> ([VirtualWindowSpace] -> [VirtualWindowSpace]) -> ([PhysicalWindowSpace] -> [PhysicalWindowSpace])
marshallSort s vSort = pScreens . vSort . vScreens where
    vScreens    = map unmarshallWindowSpace . workspacesOn s
    pScreens    = map (marshallWindowSpace s)

-- | Like 'marshallSort', but operates completely on 'PhysicalWindowSpace' with physical names.
-- Thus, 'getSortByIndex' can be used in 'xmobarPP' with no problem.
marshallSort' :: ScreenId -> ([PhysicalWindowSpace] -> [PhysicalWindowSpace]) -> ([PhysicalWindowSpace] -> [PhysicalWindowSpace])
marshallSort' s vSort = vSort . workspacesOn s

-- | Convert the tag of the 'WindowSpace' from a 'VirtualWorkspace' to a 'PhysicalWorkspace'.
marshallWindowSpace   :: ScreenId -> WindowSpace -> WindowSpace
-- | Convert the tag of the 'WindowSpace' from a 'PhysicalWorkspace' to a 'VirtualWorkspace'.
unmarshallWindowSpace :: WindowSpace -> WindowSpace

marshallWindowSpace s ws = ws { W.tag = marshall s  (W.tag ws) }
unmarshallWindowSpace ws = ws { W.tag = unmarshallW (W.tag ws) }


-- $integration
-- The @logHook@ from "XMonad.Actions.CopyWindow" needs some adjustment
-- when used with IndependentScreens.
-- To distinguish workspaces containing copies of the focused window on
-- the current screen, you can use something like:
--
-- > sampleLogHook h s = do
-- >    copies <- wsContainingCopies
-- >    let copies' = map (drop 2) copies
-- >    let check ws | ws `elem` copies' = pad . xmobarColor "red" "black" $ ws
-- >                 | otherwise = pad ws
-- >    dynamicLogWithPP $ marshallPP s myPP {ppHidden = check, ppOutput = hPutStrLn h}
-- 
-- Then use sampleLogHook in main:
-- > main = do
-- >    h <- spawnPipe "xmobar -x 0 <path to config>"
-- >    h1 <- spawnPipe "xmobar -x 1 <path to config>"
-- >    xmonad def { logHook = sampleLogHook h 0 <+> sampleLogHook h1 1 }
--
-- If the number of screens varies or is uncertain, you can use something like:
-- > main = do
-- >    n <- countScreens
-- >    hs <- traverse (\(S n) -> spawnPipe ("xmobar -x " ++ show n ++ " ~/.xmobarrc-" ++ show n)) [0..n-1]
-- >    xmonad def { logHook = mconcat (zipWith sampleLogHook hs [0..n-1]) }

-- $integration2
-- The @withNthWorkspace@ from "XMonad.Actions.DynamicWorkspaces" neeeds some adjustment.
-- When used with "XMonad.Layout.IndependentScreens", 'withNthWorkspace' can be replaced by 'withNthWorkspaceScreen'.
-- > ("M-S-1", withNthWorkspace W.shift 1)
-- can be replaced with:
-- > ("M-S-1", withNthWorkspaceScreen W.shift 1)
-- The behavior of 'withNthWorkspace' and 'withNthWorkspaceScreen' are similar on the current screen.
