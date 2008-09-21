{- |
Module      :  XMonad.Util.Paste
Author      :  Jérémy Bobbio
Copyright   :  (C) 2008
License     :  BSD3

Maintainer  :  <gwern0@gmail.com>
Stability   :  unstable
Portability :  unportable

A module for sending key presses to windows. This modules provides generalized
and specialized functions for this task.
-}

module XMonad.Util.Paste where

import XMonad (io, theRoot, withDisplay, X ())
import Graphics.X11
import Graphics.X11.Xlib.Extras (none, setEventType, setKeyEvent)
import Control.Monad.Reader (asks)
import XMonad.Operations (withFocused)
import Data.Char (isUpper)
import Graphics.X11.Xlib.Misc (stringToKeysym)
import XMonad.Util.XSelection (getSelection)


{- $usage

Import this module into your xmonad.hs as usual:

> import XMonad.Util.XPaste

And use the functions. They all return "X ()", and so are appropriate
for use as keybindings. Example:

>          , ((m,              xK_d), pasteString "foo bar") ]

Don't expect too much of the functions; they probably don't work on complex
texts.
-}

-- | Paste the current X mouse selection. Note that this uses 'getSelection' from
--   "XMonad.Util.XSelection" and so is heir to its flaws.
pasteSelection :: X ()
pasteSelection = getSelection >>= pasteString

-- | Send a string to the window with current focus. This function correctly
-- handles capitalization.
pasteString :: String -> X ()
pasteString = mapM_ (\x -> if isUpper x then pasteChar shiftMask x else pasteChar 0 x)

{- | Send a character to the current window. This is more low-level.
   Remember that you must handle the case of capitalization appropriately.
   That is, from the window's perspective:

   > pasteChar mod2Mask 'F' ~> "f"

   You would want to do something like:

   > pasteChar shiftMask 'F'
-}
pasteChar :: KeyMask -> Char -> X ()
pasteChar m c = sendKey m $ stringToKeysym [c]

sendKey :: KeyMask -> KeySym -> X ()
sendKey = (withFocused .) . sendKeyWindow

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
