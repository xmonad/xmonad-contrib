-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.KeyRemap
-- Description :  Remap Keybinding on the fly.
-- Copyright   :  (c) Christian Dietrich
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  stettberger@dokucde.de
-- Stability   :  unstable
-- Portability :  unportable
--
-- Remap Keybinding on the fly, e.g having Dvorak char, but everything with Control/Shift
-- is left us Layout
--
-----------------------------------------------------------------------------

module XMonad.Actions.KeyRemap (
  -- * Usage
  -- $usage
  setKeyRemap,
  buildKeyRemapBindings,
  setDefaultKeyRemap,

  KeymapTable (KeymapTable),
  emptyKeyRemap,
  dvorakProgrammerKeyRemap
  ) where

import XMonad
import XMonad.Prelude
import XMonad.Util.Paste

import qualified XMonad.Util.ExtensibleState as XS


newtype KeymapTable = KeymapTable [((KeyMask, KeySym), (KeyMask, KeySym))] deriving (Show)

instance ExtensionClass KeymapTable where
   initialValue = KeymapTable []

-- $usage
-- Provides the possibility to remap parts of the keymap to generate different keys
--
-- * E.g You want to type Programmers Dvorak, but your keybindings should be the normal us layout
--   after all
--
-- First, you must add all possible keybindings for all layout you want to use:
--
-- >   keys = myKeys ++ buildKeyRemapBindings [dvorakProgrammerKeyRemap,emptyKeyRemap]
--
-- Then you must add setDefaultKeyRemap to your startup hook (e.g. you want to set the
-- empty keyremap (no remapping is done) as default after startup):
--
-- > myStartupHook :: X()
-- > myStartupHook = do
-- >   setWMName "LG3D"
-- >   setDefaultKeyRemap emptyKeyRemap [dvorakProgrammerKeyRemap, emptyKeyRemap]
--
-- Then you add keybindings for changing keyboard layouts;
--
-- > , ((0                    , xK_F1    ), setKeyRemap emptyKeyRemap)
-- > , ((0                    , xK_F2    ), setKeyRemap dvorakProgrammerKeyRemap)
--
-- When defining your own keymappings, please be aware of:
--
-- * If you want to emulate a key that is shifted on us you must emulate that keypress:
--
-- > KeymapTable [((0, xK_a), (shiftMask, xK_5))] -- would bind 'a' to '%'
-- > KeymapTable [((shiftMask, xK_a), (0, xK_5))] -- would bind 'A' to '5'
--
-- * the dvorakProgrammerKeyRemap uses the original us layout as lookuptable to generate
--   the KeymapTable
--
-- * KeySym and (ord Char) are incompatible, therefore the magic numbers in dvorakProgrammerKeyRemap
--   are nessesary

doKeyRemap :: KeyMask -> KeySym -> X()
doKeyRemap mask sym = do
  table <- XS.get
  let (insertMask, insertSym) = extractKeyMapping table mask sym
  sendKey insertMask insertSym

-- | Using this in the keybindings to set the actual Key Translation table
setKeyRemap :: KeymapTable -> X()
setKeyRemap table = do
  let KeymapTable newtable = table
  KeymapTable oldtable <- XS.get
  XConf { display = dpy, theRoot = rootw } <- ask

  let grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
  let ungrab kc m = io $ ungrabKey dpy kc m rootw

  forM_ oldtable $ \((mask, sym), _) -> do
    kc <- io $ keysymToKeycode dpy sym
    -- "If the specified KeySym is not defined for any KeyCode,
    -- XKeysymToKeycode() returns zero."
    when (kc /= 0) $ ungrab kc mask

  forM_ newtable $ \((mask, sym), _) -> do
    kc <- io $ keysymToKeycode dpy sym
    -- "If the specified KeySym is not defined for any KeyCode,
    -- XKeysymToKeycode() returns zero."
    when (kc /= 0) $ grab kc mask

  XS.put table

-- | Adding this to your startupHook, to select your default Key Translation table.
--   You also must give it all the KeymapTables you are willing to use
setDefaultKeyRemap  :: KeymapTable -> [KeymapTable] -> X()
setDefaultKeyRemap dflt keyremaps = do
  XS.put (KeymapTable mappings)
  setKeyRemap dflt
  where
    mappings = nub (keyremaps >>= \(KeymapTable table) -> table)

extractKeyMapping :: KeymapTable -> KeyMask -> KeySym -> (KeyMask, KeySym)
extractKeyMapping (KeymapTable table) mask sym =
  insertKey filtered
  where filtered = filter (\((m, s),_) -> m == mask && s == sym) table
        insertKey [] = (mask, sym)
        insertKey ((_, to):_) = to

-- | Append the output of this function to your keybindings with ++
buildKeyRemapBindings :: [KeymapTable] -> [((KeyMask, KeySym), X ())]
buildKeyRemapBindings keyremaps =
  [((mask, sym), doKeyRemap mask sym) | (mask, sym) <- bindings]
  where mappings = concatMap (\(KeymapTable table) -> table) keyremaps
        bindings = nub (map fst mappings)


-- Here come the Keymappings
-- | The empty KeymapTable, does no translation
emptyKeyRemap :: KeymapTable
emptyKeyRemap = KeymapTable []

-- | The dvorak Programmers keymap, translates from us keybindings to dvorak programmers
dvorakProgrammerKeyRemap :: KeymapTable
dvorakProgrammerKeyRemap =
  KeymapTable [((charToMask maskFrom, from), (charToMask maskTo, to)) |
               (maskFrom, from, maskTo, to) <- zip4 layoutUsShift layoutUsKey layoutDvorakShift layoutDvorakKey]
  where

    layoutUs    = map (fromIntegral . fromEnum) "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"  :: [KeySym]
    layoutUsKey = map (fromIntegral . fromEnum) "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"  :: [KeySym]
    layoutUsShift = "0000000000000000000000000000000000000000000000011111111111111111111111111111111111111111111111"

    layoutDvorak = map (fromIntegral . fromEnum) "$&[{}(=*)+]!#;,.pyfgcrl/@\\aoeuidhtns-'qjkxbmwvz~%7531902468`:<>PYFGCRL?^|AOEUIDHTNS_\"QJKXBMWVZ" :: [KeySym]

    layoutDvorakShift = map getShift layoutDvorak
    layoutDvorakKey   = map getKey layoutDvorak
    getKey   char = fromJust $ (layoutUsKey   !?) =<< elemIndex char layoutUs
    getShift char = fromJust $ (layoutUsShift !?) =<< elemIndex char layoutUs
    charToMask char = if [char] == "0" then 0 else shiftMask
