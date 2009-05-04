{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.NamedActions
-- Copyright   :  Adam Vogt <vogt.adam@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Present a list of the keybindings in use.
--------------------------------------------------------------------

module XMonad.Util.NamedActions (
    -- * Usage:
    -- $usage
    sendMessage',
    spawn',
    submapName,
    addDescrKeys,
    xMessage,

    showKmSimple,
    showKm,

    noName,
    oneName,
    addName,

    (^++^),

    NamedAction(..),
    HasName,
    defaultKeysDescr
    ) where

import XMonad.Actions.Submap(submap)
import XMonad(KeySym, KeyMask, X, Layout, Message,
              XConfig(workspaces, terminal, modMask, layoutHook, keys, XConfig),
              io, spawn, whenJust, ChangeLayout(NextLayout), IncMasterN(..),
              Resize(..), kill, refresh, screenWorkspace, sendMessage, setLayout,
              windows, withFocused, controlMask, mod1Mask, mod2Mask, mod3Mask,
              mod4Mask, mod5Mask, shiftMask, xK_1, xK_9, xK_Return, xK_Tab, xK_c,
              xK_comma, xK_e, xK_h, xK_j, xK_k, xK_l, xK_m, xK_n, xK_p,
              xK_period, xK_q, xK_r, xK_space, xK_t, xK_w, keysymToString)
import System.Posix.Process(executeFile, forkProcess)
import Control.Arrow(Arrow((***), second, (&&&), first))
import Data.Bits(Bits((.|.), complement, (.&.)))
import Data.Function((.), const, ($), flip, id, on)
import Data.List((++), filter, zip, map, concatMap, elem, head,
                 last, null, unlines, groupBy, intercalate, partition, sortBy)
import System.Exit(ExitCode(ExitSuccess), exitWith)

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
-- > main = xmonad $ addDescrKeys ((mod4Mask, xK_d), xMessage) myKeys
-- >                    defaultConfig { modMask = mod4Mask }
-- >
-- > myKeys = flip mkNamedKeymap $
-- >    [("M-x a", addName "useless..." $ spawn "xmessage foo"),
-- >     ("M-c", sendMessage' Expand)]
-- >     ^++^
-- >    [("<XF86AudioPlay>", spawn "mpc toggle" :: X ()),
-- >     ("<XF86AudioNext>", spawn "mpc next"]
--
-- Due to the type of '^++^', you can combine bindings whose actions are @X ()@
-- as well as actions that have descriptions. However you cannot mix the two in
-- a single list, unless each is prefixed with 'addName' or 'noName'. '^++^'
-- works with traditional-style keybindings too.
--
-- Also note the unfortunate necessity of a type annotation, since 'spawn' is
-- too general.

deriving instance Show XMonad.Resize
deriving instance Show XMonad.IncMasterN

-- | 'sendMessage' but add a description that is @show message@. Note that not
-- all messages have show instances.
sendMessage' :: (Message a, Show a) => a -> NamedAction
sendMessage' x = NamedAction $ (XMonad.sendMessage x,show x)

-- | 'spawn' but the description is the string passed
spawn' :: String -> NamedAction
spawn' x = addName x $ spawn x

class HasName a where
    showName :: a -> [String]
    showName = const [""]
    getAction :: a -> X ()

instance HasName (X ()) where
    getAction = id

instance HasName (IO ()) where
    getAction = io

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

-- | Or allow another lookup table?
modToString :: KeyMask -> String
modToString mask = concatMap (++"-") $ filter (not . null)
                $ map (uncurry w)
                [(mod1Mask, "M1")
                ,(mod2Mask, "M2")
                ,(mod3Mask, "M3")
                ,(mod4Mask, "M4")
                ,(mod5Mask, "M5")
                ,(controlMask, "C")
                ,(shiftMask,"Shift")]
    where w m str = if m .&. complement mask == 0 then str else ""

keyToString :: (KeyMask, KeySym) -> [Char]
keyToString = uncurry (++) . (modToString *** keysymToString)

-- | Squeezes bindings from [xK_1 .. xK_9]
showKm :: [((KeyMask, KeySym), NamedAction)] -> [[Char]]
showKm = uncurry (flip (++))
         . second showKmSimple
         . first (map ( intercalate " ... " . showKmSimple . uncurry (:)
                      . (head &&& (:[]) . last)
                      . sortBy (compare `on` (snd . fst)))
                 . groupBy ((==) `on` (fst . fst))
                 )
         . partition ((`elem` [xK_1 .. xK_9]) . snd . fst)

showKmSimple :: [((KeyMask, KeySym), NamedAction)] -> [[Char]]
showKmSimple = concatMap (\(k,e) -> map ((keyToString k ++) . smartSpace) $ showName e)
    where smartSpace [] = []
          smartSpace xs = ' ':xs

-- | An action to send to 'addDescrKeys' for showing the keybindings. See also 'showKm' and 'showKmSimple'
xMessage :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
xMessage x = addName "Show Keybindings" $ io $ do
    forkProcess $ executeFile "xmessage" True ["-default", "okay", unlines $ showKm x] Nothing
    return ()

-- | Merge the supplied keys with 'defaultKeysDescr'
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

-- | A version of the default keys from 'XMonad.Config.defaultConfig', but with
-- 'NamedAction'  instead of @X ()@
defaultKeysDescr :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
defaultKeysDescr conf@(XConfig {XMonad.modMask = modm}) =
    -- launching and killing programs
    [ ((modm .|. shiftMask, xK_Return), addName "Launch Terminal" $ spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm,               xK_p     ), addName "Launch dmenu" $ spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modm .|. shiftMask, xK_p     ), addName "Launch gmrun" $ spawn "gmrun") -- %! Launch gmrun
    , ((modm .|. shiftMask, xK_c     ), addName "Close the focused window" kill) -- %! Close the focused window

    , ((modm,               xK_space ), sendMessage' NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modm,               xK_n     ), addName "Refresh" refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modm,               xK_Tab   ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_j     ), addName "Focus down" $ windows W.focusDown) -- %! Move focus to the next window
    , ((modm,               xK_k     ), addName "Focus up"   $ windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_m     ), addName "Focus the master" $ windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modm,               xK_Return), addName "Swap with the master" $ windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), addName "Swap down" $ windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), addName "Swap up"   $ windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modm,               xK_h     ), sendMessage' Shrink) -- %! Shrink the master area
    , ((modm,               xK_l     ), sendMessage' Expand) -- %! Expand the master area

    -- floating layer support
    , ((modm,               xK_t     ), addName "Push floating to tiled" $ withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modm              , xK_comma ), sendMessage' (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage' (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modm .|. shiftMask, xK_q     ), addName "Quit" $ io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modm              , xK_q     ), addName "Restart" $ spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <- [(W.greedyView, 0, "Switch to workspace "), (W.shift, shiftMask, "Move client to workspace ")]
        , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m, n) <- [(W.view, 0, "Switch to screen number "), (W.shift, shiftMask, "Move client to screen number ")]
        , (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

-- | These are just the @NamedAction@ constructor but with a more specialized
-- type, so that you don't have to supply any annotations, for ex coercing
-- spawn to @X ()@ from the more general @MonadIO m => m ()@
noName :: X () -> NamedAction
noName = NamedAction

oneName :: (X (), String) -> NamedAction
oneName = NamedAction

addName :: String -> X () -> NamedAction
addName = flip (curry NamedAction)
