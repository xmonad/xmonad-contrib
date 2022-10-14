{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.Modal
-- Description :  Implements true modality in xmonad key-bindings.
-- Copyright   :  (c) 2018  L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Author      :  L. S. Leary
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module implements modal keybindings for xmonad.
--
--------------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{
module XMonad.Hooks.Modal
  (
 -- * Usage
 -- $Usage
    modal
  , modeWithExit
  , mode
  , Mode
  , mkKeysEz
  , setMode
  , exitMode
 -- * Provided Modes #ProvidedModes#
 -- $ProvidedModes
  , noModModeLabel
  , noModMode
  , floatModeLabel
  , floatMode
  , overlayedFloatModeLabel
  , overlayedFloatMode
  , floatMap
  , overlay
 -- * Logger
  , logMode
  ) where

-- core
import           XMonad
-- base
import           Data.Bits                     ( (.&.)
                                               , complement
                                               )
import           Data.List
import qualified Data.Map.Strict               as M
-- contrib
import           XMonad.Actions.FloatKeys      ( keysMoveWindow
                                               , keysResizeWindow
                                               )
import           XMonad.Prelude
import           XMonad.Util.EZConfig          ( parseKeyCombo
                                               , mkKeymap
                                               )
import qualified XMonad.Util.ExtensibleConf    as XC
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.Grab
import           XMonad.Util.Loggers
import           XMonad.Util.Parser            ( runParser )

-- }}}

-- Original Draft By L.S.Leary : https://gist.github.com/LSLeary/6741b0572d62db3f0cea8e6618141b2f

-- --< Usage >-- {{{

-- $Usage
--
-- This module provides modal keybindings in xmonad. If you're not familiar with
-- modal keybindings from Vim, you can think of modes as submaps from
-- "XMonad.Actions.Submap", but after each action you execute, you land back in
-- the submap until you explicitly exit the submap. To use this module you
-- should apply the 'modal' function to the config, which will setup the list of
-- modes (or rather, @XConfig Layout -> Mode@) you provide:
--
-- >
-- > import XMonad
-- > import XMonad.Hooks.Modal
-- > import XMonad.Util.EZConfig
-- > import qualified Data.Map as M
-- >
-- > main :: IO ()
-- > main =
-- >   xmonad
-- >     . modal [noModMode, floatMode 10, overlayedFloatMode 10, sayHelloMode]
-- >     $ def
-- >     `additionalKeysP` [ ("M-S-n", setMode noModModeLabel)
-- >                       , ("M-S-r", setMode floatModeLabel)
-- >                       , ("M-S-z", setMode overlayedFloatModeLabel)
-- >                       , ("M-S-h", setMode "Hello")
-- >                       ]
-- >
-- > sayHelloMode :: Mode
-- > sayHelloMode = mode "Hello" $ mkKeysEz
-- >   [ ("h", xmessage "Hello, World!")
-- >   , ("M-g", xmessage "Goodbye, World!")
-- >   ]
--
-- Alternatively, one could have defined @sayHelloMode@ as
--
-- > sayHelloMode :: Mode
-- > sayHelloMode = mode "Hello" $ \cfg ->
-- >   M.fromList [ ((noModMask, xK_h), xmessage "Hello, World!")
-- >              , ((modMask cfg, xK_g), xmessage "Goodbye, World!")
-- >              ]
--
-- In short, a 'Mode' has a label describing its purpose, as well as
-- attached keybindings. These are of the form
--
--   - @[(String, X ())]@, or
--
--   - @XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())@.
--
-- The former—accessible via 'mkKeysEz'—is how specifying keys work with
-- "XMonad.Util.EZConfig", while the latter is more geared towards how
-- defining keys works by default in xmonad. Note that, by default,
-- modes are exited with the Escape key. If one wishes to customise
-- this, the 'modeWithExit' function should be used instead of 'mode'
-- when defining a new mode.
--
-- The label of the active mode can be logged with 'logMode' to be
-- displayed in a status bar, for example (For more information check
-- "XMonad.Util.Loggers"). Some examples are included in [the provided
-- modes](#g:ProvidedModes).

-- }}}

-- --< Types >-- {{{

-- | From a list of 'XMonad.Util.EZConfig'-style bindings, generate a
-- key representation.
--
-- >>> mkKeysEz [("h", xmessage "Hello, world!")]
mkKeysEz :: [(String, X ())] -> (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
mkKeysEz = flip mkKeymap

-- | The mode type. Use 'mode' or 'modeWithExit' to create modes.
data Mode = Mode
  { label     :: !String
  , boundKeys :: !(XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
  }

-- | Newtype for the extensible config.
newtype ModeConfig = MC [Mode] deriving Semigroup

-- | Newtype for the extensible state.
newtype CurrentMode = CurrentMode
  { currentMode :: Maybe Mode
  }

instance ExtensionClass CurrentMode where
  initialValue = CurrentMode Nothing

-- }}}

-- --< Private >-- {{{

-- | The active keybindings corresponding to the active 'Mode' (or lack
-- thereof).
currentKeys :: X (M.Map (ButtonMask, KeySym) (X ()))
currentKeys = do
  cnf <- asks config
  XS.gets currentMode >>= \case
    Just m  -> pure (boundKeys m cnf)
    Nothing -> join keys <$> asks config

-- | Grab the keys corresponding to the active 'Mode' (or lack thereof).
regrab :: X ()
regrab = grab . M.keys =<< currentKeys

-- | Called after changing the mode. Grabs the correct keys and runs the
-- 'logHook'.
refreshMode :: X ()
refreshMode = regrab >> asks config >>= logHook

-- | Event hook to control the keybindings.
modalEventHook :: Event -> X All
modalEventHook = customRegrabEvHook regrab <> \case
  KeyEvent { ev_event_type = t, ev_state = m, ev_keycode = code }
    | t == keyPress -> withDisplay $ \dpy -> do
      kp  <- (,) <$> cleanMask m <*> io (keycodeToKeysym dpy code 0)
      kbs <- currentKeys
      userCodeDef () (whenJust (M.lookup kp kbs) id)
      pure (All False)
  _ -> pure (All True)

-- }}}

-- --< Public >-- {{{

-- | Adds the provided modes to the user's config, and sets up the bells
-- and whistles needed for them to work.
modal :: [Mode] -> XConfig l -> XConfig l
modal modes = XC.once
  (\cnf -> cnf { startupHook     = startupHook cnf <> initModes
               , handleEventHook = handleEventHook cnf <> modalEventHook
               }
  )
  (MC modes)
  where initModes = XS.put (CurrentMode Nothing) >> refreshMode

-- | Create a 'Mode' from the given binding to 'exitMode', label and
-- keybindings.
modeWithExit :: String -> String -> (XConfig Layout -> M.Map (KeyMask, KeySym) (X ())) -> Mode
modeWithExit exitKey mlabel keys = Mode mlabel $ \cnf ->
  let exit = fromMaybe (0, xK_Escape) $ runParser (parseKeyCombo cnf) exitKey
   in M.insert exit exitMode (keys cnf)

-- | Create a 'Mode' from the given label and keybindings. Sets the
-- @escape@ key to 'exitMode'.
mode :: String -> (XConfig Layout -> M.Map (KeyMask, KeySym) (X ())) -> Mode
mode = modeWithExit "<Escape>"

-- | Set the current 'Mode' based on its label.
setMode :: String -> X ()
setMode l = do
  XC.with $ \(MC ls) -> case find ((== l) . label) ls of
    Nothing -> mempty
    Just m  -> do
      XS.modify $ \cm -> cm { currentMode = Just m }
      refreshMode

-- | Exits the current mode.
exitMode :: X ()
exitMode = do
  XS.modify $ \m -> m { currentMode = Nothing }
  refreshMode

-- | A 'Logger' to display the current mode.
logMode :: Logger
logMode = fmap label <$> XS.gets currentMode

-- Provided modes
noModModeLabel, floatModeLabel, overlayedFloatModeLabel :: String
noModModeLabel = "NoMod"
floatModeLabel = "Float"
overlayedFloatModeLabel = "Overlayed Float"

-- | In this 'Mode', all keybindings are available without the need for pressing
-- the modifier. Pressing @escape@ exits the mode.
noModMode :: Mode
noModMode =
  mode noModModeLabel $ \cnf -> stripModifier (modMask cnf) (keys cnf cnf)

-- | Generates the keybindings for 'floatMode' and 'overlayedFloatMode'.
floatMap
  :: KeyMask -- ^ Move mask
  -> KeyMask -- ^ Enlarge mask
  -> KeyMask -- ^ Shrink mask
  -> Int -- ^ Step size
  -> M.Map (ButtonMask, KeySym) (X ())
floatMap move enlarge shrink s = M.fromList
  [ -- move
    ((move, xK_h)          , withFocused (keysMoveWindow (-s, 0)))
  , ((move, xK_j)          , withFocused (keysMoveWindow (0, s)))
  , ((move, xK_k)          , withFocused (keysMoveWindow (0, -s)))
  , ((move, xK_l)          , withFocused (keysMoveWindow (s, 0)))
  -- enlarge
  , ((enlarge, xK_h), withFocused (keysResizeWindow (s, 0) (1, 0)))
  , ((enlarge, xK_j), withFocused (keysResizeWindow (0, s) (0, 0)))
  , ((enlarge, xK_k), withFocused (keysResizeWindow (0, s) (0, 1)))
  , ((enlarge, xK_l), withFocused (keysResizeWindow (s, 0) (0, 0)))
  -- shrink
  , ((shrink, xK_h), withFocused (keysResizeWindow (-s, 0) (0, 0)))
  , ((shrink, xK_j), withFocused (keysResizeWindow (0, -s) (0, 1)))
  , ((shrink, xK_k), withFocused (keysResizeWindow (0, -s) (0, 0)))
  , ((shrink, xK_l), withFocused (keysResizeWindow (-s, 0) (1, 0)))
  , ((noModMask, xK_Escape), exitMode)
  ]

-- | A mode to control floating windows with @{hijk}@, @M-{hijk}@ and
-- @M-S-{hijk}@ in order to respectively move, enlarge and
-- shrink windows.
floatMode
  :: Int -- ^ Step size
  -> Mode
floatMode i = mode floatModeLabel $ \XConfig { modMask } ->
  floatMap noModMask modMask (modMask .|. shiftMask) i

-- | Similar to 'resizeMode', but keeps the bindings of the original
-- config active.
overlayedFloatMode
  :: Int -- ^ Step size
  -> Mode
overlayedFloatMode = overlay overlayedFloatModeLabel . floatMode

-- | Modifies a mode so that the keybindings are merged with those from
-- the config instead of replacing them.
overlay
  :: String  -- ^ Label for the new mode
  -> Mode -- ^ Base mode
  -> Mode
overlay label m = Mode label $ \cnf -> boundKeys m cnf <> keys cnf cnf

-- | Strips the modifier key from the provided keybindings.
stripModifier
  :: ButtonMask -- ^ Modifier to remove
  -> M.Map (ButtonMask, KeySym) (X ()) -- ^ Original keybinding map
  -> M.Map (ButtonMask, KeySym) (X ())
stripModifier mask = M.mapKeys $ \(m, k) -> (m .&. complement mask, k)

-- }}}
