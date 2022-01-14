{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances, StandaloneDeriving, TupleSections #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.NamedActions
-- Description :  A wrapper for keybinding configuration that can list the available keybindings.
-- Copyright   :  2009 Adam Vogt <vogt.adam@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A wrapper for keybinding configuration that can list the available
-- keybindings.
--
-- Note that xmonad>=0.11 has by default a list of the default keybindings
-- bound to @M-S-/@ or @M-?@.
--------------------------------------------------------------------

module XMonad.Util.NamedActions (
    -- * Usage:
    -- $usage
    sendMessage',
    spawn',
    submapName,
    addDescrKeys,
    addDescrKeys',
    xMessage,

    showKmSimple,
    showKm,

    noName,
    oneName,
    addName,

    separator,
    subtitle,

    (^++^),

    NamedAction(..),
    HasName,
    defaultKeysDescr
    ) where


import XMonad.Actions.Submap(submap)
import XMonad.Prelude (groupBy, keyToString)
import XMonad
import Control.Arrow(Arrow((&&&), second))
import System.Exit(exitSuccess)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- $usage
-- Here is an example config that demonstrates the usage of 'sendMessage'',
-- 'mkNamedKeymap', 'addDescrKeys', and '^++^'
--
-- > import XMonad
-- > import XMonad.Util.NamedActions
-- > import XMonad.Util.EZConfig
-- >
-- > main = xmonad $ addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys
-- >                    def { modMask = mod4Mask }
-- >
-- > myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
-- >    [("M-x a", addName "useless message" $ spawn "xmessage foo"),
-- >     ("M-c", sendMessage' Expand)]
-- >     ^++^
-- >    [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
-- >     ("<XF86AudioNext>", spawn "mpc next")]
--
-- Using '^++^', you can combine bindings whose actions are @X ()@
-- as well as actions that have descriptions. However you cannot mix the two in
-- a single list, unless each is prefixed with 'addName' or 'noName'.
--
-- If you don't like EZConfig, you can still use '^++^' with the basic XMonad
-- keybinding configuration too.
--
-- Also note the unfortunate necessity of a type annotation, since 'spawn' is
-- too general.

-- TODO: squeeze titles that have no entries (consider titles containing \n)
--
-- Output to Multiple columns
--
-- Devin Mullin's suggestions:
--
-- Reduce redundancy wrt mkNamedSubmaps, mkSubmaps and mkNamedKeymap to have a
-- HasName context (and leave mkKeymap as a specific case of it?)
--    Currently kept separate to aid error messages, common lines factored out
--
-- Suggestions for UI:
--
-- - An IO () -> IO () that wraps the main xmonad action and wrests control
--   from it if the user asks for --keys.
--
-- Just a separate binary: keep this as the only way to show keys for simplicity
--
-- - An X () that toggles a cute little overlay like the ? window for gmail
--   and reader.
--
-- Add dzen binding

deriving instance Show XMonad.Resize
deriving instance Show XMonad.IncMasterN

-- | 'sendMessage' but add a description that is @show message@. Note that not
-- all messages have show instances.
sendMessage' :: (Message a, Show a) => a -> NamedAction
sendMessage' x = NamedAction (XMonad.sendMessage x,show x)

-- | 'spawn' but the description is the string passed
spawn' :: String -> NamedAction
spawn' x = addName x $ spawn x

class HasName a where
    {-# MINIMAL getAction #-}
    showName :: a -> [String]
    showName = const [""]
    getAction :: a -> X ()

instance HasName (X ()) where
    getAction = id

instance HasName (IO ()) where
    getAction = io

instance HasName [Char] where
    getAction _ = return ()
    showName = (:[])

instance HasName (X (),String) where
    showName = (:[]) . snd
    getAction = fst

instance HasName (X (),[String]) where
    showName = snd
    getAction = fst

-- show only the outermost description
instance HasName (NamedAction,String) where
    showName = (:[]) . snd
    getAction = getAction . fst

instance HasName NamedAction where
    showName (NamedAction x) = showName x
    getAction (NamedAction x) = getAction x

-- | An existential wrapper so that different types can be combined in lists,
-- and maps
data NamedAction = forall a. HasName a => NamedAction a

-- | 'submap', but propagate the descriptions of the actions. Does this belong
-- in "XMonad.Actions.Submap"?
submapName :: (HasName a) => [((KeyMask, KeySym), a)] -> NamedAction
submapName = NamedAction . (submap . M.map getAction . M.fromList &&& showKm)
                . map (second NamedAction)

-- | Combine keymap lists with actions that may or may not have names
(^++^) :: (HasName b, HasName b1) =>
     [(d, b)] -> [(d, b1)] -> [(d, NamedAction)]
a ^++^ b = map (second NamedAction) a ++ map (second NamedAction) b

showKmSimple :: [((KeyMask, KeySym), NamedAction)] -> [[Char]]
showKmSimple = concatMap (\(k,e) -> if snd k == 0 then "":showName e else map ((keyToString k ++) . smartSpace) $ showName e)

smartSpace :: String -> String
smartSpace [] = []
smartSpace xs = ' ':xs

_test :: String
_test = unlines $ showKm $ defaultKeysDescr XMonad.def { XMonad.layoutHook = XMonad.Layout $ XMonad.layoutHook XMonad.def }

showKm :: [((KeyMask, KeySym), NamedAction)] -> [String]
showKm keybindings = padding $ do
    (k,e) <- keybindings
    if snd k == 0 then map ("",) $ showName e
        else map ((,) (keyToString k) . smartSpace) $ showName e
    where padding = let pad n (k,e) = if null k then "\n>> "++e else take n (k++repeat ' ') ++ e
                        expand xs n = map (pad n) xs
                        getMax = map (maximum . map (length . fst))
            in concat . (zipWith expand <*> getMax) . groupBy (const $ not . null . fst)

-- | An action to send to 'addDescrKeys' for showing the keybindings. See also 'showKm' and 'showKmSimple'
xMessage :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
xMessage x = addName "Show Keybindings" $ xmessage $ unlines $ showKm x

-- | Merge the supplied keys with 'defaultKeysDescr', also adding a keybinding
-- to run an action for showing the keybindings.
addDescrKeys :: (HasName b1, HasName b) =>
    ((KeyMask, KeySym),[((KeyMask, KeySym), NamedAction)] -> b)
    -> (XConfig Layout -> [((KeyMask, KeySym), b1)])
    -> XConfig l
    -> XConfig l
addDescrKeys k ks = addDescrKeys' k (\l -> defaultKeysDescr l ^++^ ks l)

-- | Without merging with 'defaultKeysDescr'
addDescrKeys' :: (HasName b) =>
    ((KeyMask, KeySym),[((KeyMask, KeySym), NamedAction)] -> b)
    -> (XConfig Layout -> [((KeyMask, KeySym), NamedAction)]) -> XConfig l -> XConfig l
addDescrKeys' (k,f) ks conf =
    let shk l = f $ [(k,f $ ks l)] ^++^ ks l
        keylist l = M.map getAction $ M.fromList $ ks l ^++^ [(k, shk l)]
    in conf { keys = keylist }

-- | A version of the default keys from the default configuration, but with
-- 'NamedAction'  instead of @X ()@
defaultKeysDescr :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
defaultKeysDescr conf@XConfig{XMonad.modMask = modm} =
    [ subtitle "launching and killing programs"
    , ((modm .|. shiftMask, xK_Return), addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm,               xK_p     ), addName "Launch dmenu" $ spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modm .|. shiftMask, xK_p     ), addName "Launch gmrun" $ spawn "gmrun") -- %! Launch gmrun
    , ((modm .|. shiftMask, xK_c     ), addName "Close the focused window" kill) -- %! Close the focused window

    , subtitle "changing layouts"
    , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , separator
    , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

    , subtitle "move focus up or down the window stack"
    , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_j     ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm,               xK_k     ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_m     ), addName "Focus the master" $ windows W.focusMaster  ) -- %! Move focus to the master window

    , subtitle "modifying the window order"
    , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

    , subtitle "resizing the master/slave ratio"
    , ((modm,               xK_h     ), sendMessage' Shrink) -- %! Shrink the master area
    , ((modm,               xK_l     ), sendMessage' Expand) -- %! Expand the master area

    , subtitle "floating layer support"
    , ((modm,               xK_t     ), addName "Push floating to tiled" $ withFocused $ windows . W.sink) -- %! Push window back into tiling

    , subtitle "change the number of windows in the master area"
    , ((modm              , xK_comma ), sendMessage' (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage' (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    , subtitle "quit, or restart"
    , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io exitSuccess) -- %! Quit xmonad
    , ((modm              , xK_q     ), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
    ]

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    subtitle "switching workspaces":
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
   ++
   subtitle "switching screens" :
   [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

-- | For a prettier presentation: keymask, keysym of 0 are reserved for this
-- purpose: they do not happen, afaik, and keysymToString 0 would raise an
-- error otherwise
separator :: ((KeyMask,KeySym), NamedAction)
separator = ((0,0), NamedAction (return () :: X (),[] :: [String]))

subtitle ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle x = ((0,0), NamedAction $ x ++ ":")

-- | These are just the @NamedAction@ constructor but with a more specialized
-- type, so that you don't have to supply any annotations, for ex coercing
-- spawn to @X ()@ from the more general @MonadIO m => m ()@
noName :: X () -> NamedAction
noName = NamedAction

oneName :: (X (), String) -> NamedAction
oneName = NamedAction

addName :: String -> X () -> NamedAction
addName = flip (curry NamedAction)
