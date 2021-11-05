-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Window
-- Description :  A prompt for bringing windows to you, and bring you to windows.
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
    windowMultiPrompt,
    allWindows,
    allApplications,
    wsWindows,
    XWindowMap,

    -- * Deprecated
    windowPromptGoto,
    windowPromptBring,
    windowPromptBringCopy,
    ) where

import XMonad.Prelude (forM)
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
data WindowPrompt = Goto | Bring | BringCopy | BringToMaster | WithWindow String (Window ->  X())
instance XPrompt WindowPrompt where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    showXPrompt BringToMaster
                          = "Bring window to master: "
    showXPrompt BringCopy = "Bring a copy: "
    showXPrompt (WithWindow xs _) = xs
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

-- | Internal type used for the multiple mode prompt.
data WindowModePrompt =
  WindowModePrompt WindowPrompt (M.Map String Window) (String -> String -> Bool)

instance XPrompt WindowModePrompt where
    showXPrompt (WindowModePrompt action _ _) =
        showXPrompt action

    completionFunction (WindowModePrompt _ winmap predicate) s =
        return . filter (predicate s) . map fst . M.toList $ winmap

    modeAction (WindowModePrompt action winmap _) buf auto = do
        let name = if null auto then buf else auto
            a = case action of
                  Goto           -> gotoAction
                  Bring          -> bringAction
                  BringCopy      -> bringCopyAction
                  BringToMaster  -> bringToMaster
                  WithWindow _ f -> withWindow f
        a name
      where
        withWindow f     = flip whenJust f . flip M.lookup winmap
        winAction a      = withWindow (windows . a)
        gotoAction       = winAction W.focusWindow
        bringAction      = winAction bringWindow
        bringCopyAction  = winAction bringCopyWindow
        bringToMaster    = winAction (\w s -> W.shiftMaster . W.focusWindow w $ bringWindow w s)

-- | Deprecated. Use windowPrompt instead.
{-# DEPRECATED windowPromptGoto      "Use windowPrompt instead." #-}
{-# DEPRECATED windowPromptBring     "Use windowPrompt instead." #-}
{-# DEPRECATED windowPromptBringCopy "Use windowPrompt instead." #-}
windowPromptGoto, windowPromptBring, windowPromptBringCopy :: XPConfig -> X ()
windowPromptGoto c = windowPrompt c Goto windowMap
windowPromptBring c = windowPrompt c Bring windowMap
windowPromptBringCopy c = windowPrompt c BringCopy windowMap

-- | A helper to get the map of all windows.
allWindows :: XWindowMap
allWindows = windowMap

-- | A helper to get the map of all applications
allApplications :: XWindowMap
allApplications = windowAppMap

-- | A helper to get the map of windows of the current workspace.
wsWindows :: XWindowMap
wsWindows = withWindowSet (return . W.index) >>= winmap
    where
      winmap = fmap M.fromList . mapM pair
      pair w = do name <- show <$> getName w
                  return (name, w)

-- | A Map where keys are pretty printable window names and values are
-- Xmonad windows identifier.
type XWindowMap = X (M.Map String Window)

-- | Pops open a prompt with window titles belonging to
-- winmap. Choose one, and an action is applied on the
-- selected window, according to WindowPrompt.
windowPrompt :: XPConfig -> WindowPrompt -> XWindowMap -> X ()
windowPrompt c t winmap = do
  wm <- winmap
  let mode     = WindowModePrompt t wm (searchPredicate c)
      action   = modeAction mode
      compList = completionFunction mode
  mkXPrompt t c compList (\s -> action s s)

-- | Like 'windowPrompt', but uses the multiple modes feature of
-- @Prompt@ (via 'mkXPromptWithModes').
--
-- Given a list of actions along with the windows they should work
-- with, display the appropriate prompt with the ability to switch
-- between them using the @changeModeKey@.
--
-- For example, to have a prompt that first shows you all windows, but
-- allows you to narrow the list down to just the windows on the
-- current workspace:
--
-- > windowMultiPrompt config [(Goto, allWindows), (Goto, wsWindows)]
windowMultiPrompt :: XPConfig -> [(WindowPrompt, XWindowMap)] -> X ()
windowMultiPrompt c modes = do
  modes' <- forM modes $ \(t, wm) -> do
    wm' <- wm
    return . XPT $ WindowModePrompt t wm' (searchPredicate c)

  mkXPromptWithModes modes' c

-- | Brings a copy of the specified window into the current workspace.
bringCopyWindow :: Window -> WindowSet -> WindowSet
bringCopyWindow w ws = copyWindow w (W.currentTag ws) ws
