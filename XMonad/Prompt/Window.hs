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
    windowPromptGoto,
    windowPromptBring,
    windowPromptBringCopy,
    WindowPrompt,
    ) where

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import XMonad
import XMonad.Prompt
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer

-- $usage
-- WindowPrompt brings windows to you and you to windows.
-- That is to say, it pops up a prompt with window names, in case you forgot
-- where you left your XChat.
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Window
--
-- and in the keys definition:
--
-- > , ((modm .|. shiftMask, xK_g     ), windowPromptGoto  def)
-- > , ((modm .|. shiftMask, xK_b     ), windowPromptBring def)
--
-- The autoComplete option is a handy complement here:
--
-- > , ((modm .|. shiftMask, xK_g     ), windowPromptGoto
-- >                                            def { autoComplete = Just 500000 } )
--
-- The \'500000\' is the number of microseconds to pause before sending you to
-- your new window. This is useful so that you don't accidentally send some
-- keystrokes to the selected client.
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data WindowPrompt = Goto | Bring | BringCopy
instance XPrompt WindowPrompt where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    showXPrompt BringCopy = "Bring a copy: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

windowPromptGoto, windowPromptBring, windowPromptBringCopy :: XPConfig -> X ()
windowPromptGoto  = doPrompt Goto
windowPromptBring = doPrompt Bring
windowPromptBringCopy = doPrompt BringCopy

-- | Pops open a prompt with window titles. Choose one, and you will be
-- taken to the corresponding workspace.
doPrompt :: WindowPrompt -> XPConfig -> X ()
doPrompt t c = do
  a <- case t of
         Goto  -> fmap gotoAction  windowMap
         Bring -> fmap bringAction windowMap
         BringCopy -> fmap bringCopyAction windowMap
  wm <- windowMap
  mkXPrompt t c (compList wm) a

    where
      winAction a m    = flip whenJust (windows . a) . flip M.lookup m
      gotoAction       = winAction W.focusWindow
      bringAction      = winAction bringWindow
      bringCopyAction  = winAction bringCopyWindow

      compList m s = return . filter (searchPredicate c s) . map fst . M.toList $ m


-- | Brings a copy of the specified window into the current workspace.
bringCopyWindow :: Window -> WindowSet -> WindowSet
bringCopyWindow w ws = copyWindow w (W.currentTag ws) ws
