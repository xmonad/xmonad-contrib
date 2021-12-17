{- |
Module      :  XMonad.Util.Paste
Description :  A module for sending key presses to windows.
Copyright   :  (C) 2008 Jérémy Bobbio, gwern
License     :  BSD3

Maintainer  :  none
Stability   :  unstable
Portability :  unportable

A module for sending key presses to windows. This modules provides generalized
and specialized functions for this task.
-}

module XMonad.Util.Paste ( -- * Usage
                           -- $usage
                           pasteSelection,
                           pasteString,
                           pasteChar,
                           sendKey,
                           sendKeyWindow,
                           noModMask
                         )
                           where

import XMonad (io, theRoot, withDisplay, X ())
import Graphics.X11
import Graphics.X11.Xlib.Extras (none, setEventType, setKeyEvent)
import Control.Monad.Reader (asks)
import XMonad.Operations (withFocused)
import XMonad.Prelude (isUpper, fromMaybe)
import XMonad.Util.XSelection (getSelection)
import XMonad.Util.EZConfig (parseKey)
import XMonad.Util.Parser (runParser)

{- $usage

Import this module into your xmonad.hs as usual:

> import XMonad.Util.Paste

And use the functions. They all return 'X' (), and so are appropriate
for use as keybindings. Example:

>          , ((m,              xK_d), pasteString "foo bar") ]

Don't expect too much of the functions; they probably don't work on complex
texts.
-}

-- | Paste the current X mouse selection. Note that this uses 'getSelection' from
--   "XMonad.Util.XSelection" and so is heir to its flaws.
pasteSelection :: X ()
pasteSelection = getSelection >>= pasteString

-- | Send a string to the window which is currently focused. This function correctly
-- handles capitalization. Warning: in dealing with capitalized characters, this assumes a QWERTY layout.
pasteString :: String -> X ()
pasteString = mapM_ (\x -> if isUpper x || x `elem` "~!@#$%^&*()_+{}|:\"<>?" then pasteChar shiftMask x else pasteChar noModMask x)

{- | Send a character to the current window. This is more low-level.
   Remember that you must handle the case of capitalization appropriately.
   That is, from the window's perspective:

   > pasteChar mod2Mask 'F' ~> "f"

   You would want to do something like:

   > pasteChar shiftMask 'F'

   Note that this function will probably have trouble with any 'Char'
   outside ASCII.
-}
pasteChar :: KeyMask -> Char -> X ()
pasteChar m c = sendKey m $ fromMaybe (unicodeToKeysym c)
                $ runParser parseKey [c]

-- | Send a key with a modifier to the currently focused window.
sendKey :: KeyMask -> KeySym -> X ()
sendKey = (withFocused .) . sendKeyWindow

-- | The primitive. Allows you to send any combination of 'KeyMask' and 'KeySym' to any 'Window' you specify.
sendKeyWindow :: KeyMask -> KeySym -> Window -> X ()
sendKeyWindow mods key w = withDisplay $ \d -> do
              rootw <- asks theRoot
              keycode <- io $ keysymToKeycode d key
              io $ allocaXEvent $ \ev -> do
                  setEventType ev keyPress
                  setKeyEvent ev w rootw none mods keycode True
                  sendEvent d w True keyPressMask ev
                  setEventType ev keyRelease
                  sendEvent d w True keyReleaseMask ev

-- | Convert a unicode character to a 'KeySym'. Ideally, this should
-- work for any unicode character, but see here for details:
-- http://www.cl.cam.ac.uk/~mgk25/ucs/keysyms.txt
unicodeToKeysym :: Char -> KeySym
unicodeToKeysym c
  | (ucp >= 32)  && (ucp <= 126) = fromIntegral ucp
  | (ucp >= 160) && (ucp <= 255) = fromIntegral ucp
  | ucp >= 256                   = fromIntegral $ ucp + 0x1000000
  | otherwise                    = 0 -- this is supposed to be an error, but it's not ideal
  where ucp = fromEnum c -- codepoint
