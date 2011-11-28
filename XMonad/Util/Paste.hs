{- |
Module      :  XMonad.Util.Paste
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
import Data.Char (isUpper)
import Data.Maybe (listToMaybe)
import XMonad.Util.XSelection (getSelection)
import XMonad.Util.EZConfig (parseKey)
import Text.ParserCombinators.ReadP (readP_to_S)

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
-- handles capitalization.
pasteString :: String -> X ()
pasteString = mapM_ (\x -> if isUpper x then pasteChar shiftMask x else pasteChar noModMask x)

{- | Send a character to the current window. This is more low-level.
   Remember that you must handle the case of capitalization appropriately.
   That is, from the window's perspective:

   > pasteChar mod2Mask 'F' ~> "f"

   You would want to do something like:

   > pasteChar shiftMask 'F'

   Note that this function makes use of 'stringToKeysym', and so will probably
   have trouble with any 'Char' outside ASCII.
-}
pasteChar :: KeyMask -> Char -> X ()
pasteChar m c = sendKey m $ maybe (stringToKeysym [c]) fst
                $ listToMaybe $ readP_to_S parseKey [c]

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

-- | A null 'KeyMask'. Used when you don't want a character or string shifted, control'd, or what.
--
--   TODO: This really should be a function in the X11 binding. When noModMask shows up there, remove.
noModMask :: KeyMask
noModMask = 0
