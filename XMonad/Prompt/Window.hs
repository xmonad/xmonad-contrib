-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Window
-- Copyright   :  Devin Mullins <me@twifkak.com>
--                Andrea Rossato <andrea.rossato@unibz.it>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Devin  Mullins <me@twifkak.com>
--                Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xprompt operations to bring windows to you, and bring you to windows.
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Window
    (
    -- * Usage
    -- $usage
    WindowPrompt(..),
    windowPrompt,
    allWindows,
    wsWindows,
    XWindowMap,

    -- * Deprecated
    windowPromptGoto,
    windowPromptBring,
    windowPromptBringCopy,
    ) where

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import XMonad
import XMonad.Prompt
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer
import XMonad.Util.NamedWindows

-- $usage
-- WindowPrompt brings windows to you and you to windows. That is to
-- say, it pops up a prompt with window names, in case you forgot
-- where you left your XChat. It also offers helpers to build the
-- subset of windows which is used for the prompt completion.
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Window
--
-- and in the keys definition:
--
-- > , ((modm .|. shiftMask, xK_g     ), windowPrompt def Goto wsWindows)
-- > , ((modm .|. shiftMask, xK_b     ), windowPrompt def Bring allWindows)
--
-- The autoComplete option is a handy complement here:
--
-- > , ((modm .|. shiftMask, xK_g     ), windowPrompt
-- >                                        def { autoComplete = Just 500000 }
-- >                                        Goto allWindows)
--
-- The \'500000\' is the number of microseconds to pause before sending you to
-- your new window. This is useful so that you don't accidentally send some
-- keystrokes to the selected client.
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- Describe actions that can applied  on the selected window
data WindowPrompt = Goto | Bring | BringCopy | BringToMaster
instance XPrompt WindowPrompt where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    showXPrompt BringToMaster
                          = "Bring window to master: "
    showXPrompt BringCopy = "Bring a copy: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

-- | Deprecated. Use windowPrompt instead.
windowPromptGoto, windowPromptBring, windowPromptBringCopy :: XPConfig -> X ()
windowPromptGoto c = windowPrompt c Goto windowMap
windowPromptBring c = windowPrompt c Bring windowMap
windowPromptBringCopy c = windowPrompt c BringCopy windowMap

-- | A helper to get the map of all windows.
allWindows :: XWindowMap
allWindows = windowMap

-- | A helper to get the map of windows of the current workspace.
wsWindows :: XWindowMap
wsWindows = withWindowSet (return . W.index) >>= winmap
    where
      winmap = fmap M.fromList . mapM pair
      pair w = do name <- fmap show $ getName w
                  return (name, w)

-- | A Map where keys are pretty printable window names and values are
-- Xmonad windows identifier.
type XWindowMap = X (M.Map String Window)

-- | Pops open a prompt with window titles belonging to
-- winmap. Choose one, and an action is applied on the
-- selected window, according to WindowPrompt.
windowPrompt :: XPConfig -> WindowPrompt -> XWindowMap -> X ()
windowPrompt c t winmap = do
  a <- case t of
         Goto  -> fmap gotoAction  winmap
         Bring -> fmap bringAction winmap
         BringCopy -> fmap bringCopyAction winmap
         BringToMaster -> fmap bringToMaster winmap
  wm <- winmap
  mkXPrompt t c (compList wm) a

    where
      winAction a m    = flip whenJust (windows . a) . flip M.lookup m
      gotoAction       = winAction W.focusWindow
      bringAction      = winAction bringWindow
      bringCopyAction  = winAction bringCopyWindow
      bringToMaster    = winAction (\w s -> W.shiftMaster . W.focusWindow w $ bringWindow w s)

      compList m s = return . filter (searchPredicate c s) . map fst . M.toList $ m

-- | Brings a copy of the specified window into the current workspace.
bringCopyWindow :: Window -> WindowSet -> WindowSet
bringCopyWindow w ws = copyWindow w (W.currentTag ws) ws
