{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.Widgets
-- Description :  Definitions for decoration widgets (window buttons etc)
-- Copyright   :  2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module contains data types and utilities to deal with decoration
-- widgets. A widget is anything that is displayed on window decoration,
-- and, optionally, can react on clicks. Examples of widgets are usual
-- window buttons (minimize, maximize, close), window icon and window title.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.Widgets (
    -- * Data types
    StandardCommand (..),
    TextWidget (..),
    GenericWidget (..),
    StandardWidget,
    -- * Utility functions
    isWidgetChecked,
    -- * Presets for standard widgets
    titleW, toggleStickyW, minimizeW,
    maximizeW, closeW, dwmpromoteW,
    moveToNextGroupW,moveToPrevGroupW
  ) where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.DwmPromote
import qualified XMonad.Actions.CopyWindow as CW
import qualified XMonad.Layout.Groups.Examples as Ex
import XMonad.Layout.Maximize
import XMonad.Actions.Minimize
import XMonad.Actions.WindowMenu

import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Engine

-- | Standard window commands.
--
-- One can extend this list by simply doing
--
-- > data MyWindowCommand =
-- >     Std StandardCommand
-- >   | SomeFancyCommand
--
-- > instance WindowCommand MyWindowCommand where ...
--
-- > type MyWidget = GenericWidget MyWindowCommand
--
data StandardCommand =
      FocusWindow      -- ^ Focus the window
    | FocusUp          -- ^ Move focus to previous window
    | FocusDown        -- ^ Move focus to following window
    | MoveToNextGroup  -- ^ Move the window to the next group (see "XMonad.Layout.Groups")
    | MoveToPrevGroup  -- ^ Move the window to the previous group
    | DwmPromote       -- ^ Execute @dwmpromote@ (see "XMonad.Actions.DwmPromote")
    | ToggleSticky     -- ^ Make window sticky or unstick it (see "XMonad.Actions.CopyWindow")
    | ToggleMaximize   -- ^ Maximize or restore window (see "XMonad.Layout.Maximize")
    | Minimize         -- ^ Minimize window (see "XMonad.Actions.Minimize")
    | CloseWindow      -- ^ Close the window
    | GridWindowMenu   -- ^ Show window menu via "XMonad.Actions.GridSelect" (see "XMonad.Actions.WindowMenu")
  deriving (Eq, Show, Read)

instance WindowCommand StandardCommand where
  executeWindowCommand FocusWindow w = do
    focus w
    return False
  executeWindowCommand FocusUp _ = do
    windows W.focusUp
    withFocused maximizeWindowAndFocus
    return True
  executeWindowCommand FocusDown _ = do
    windows W.focusDown
    withFocused maximizeWindowAndFocus
    return True
  executeWindowCommand MoveToNextGroup w = do
    focus w
    Ex.moveToGroupDown False
    return True
  executeWindowCommand MoveToPrevGroup w = do
    focus w
    Ex.moveToGroupUp False
    return True
  executeWindowCommand CloseWindow w = do
    killWindow w
    return True
  executeWindowCommand DwmPromote w = do
    focus w
    dwmpromote
    return True
  executeWindowCommand ToggleSticky w = do
    focus w
    copies <- CW.wsContainingCopies
    if null copies
      then windows CW.copyToAll
      else CW.killAllOtherCopies
    return True
  executeWindowCommand ToggleMaximize w = do
    sendMessage $ maximizeRestore w
    focus w
    return True
  executeWindowCommand Minimize w = do
    minimizeWindow w
    return True
  executeWindowCommand GridWindowMenu w = do
    focus w
    windowMenu
    return True

  isCommandChecked FocusWindow _ = return False
  isCommandChecked DwmPromote w = do
      withWindowSet $ \ws -> return $ Just w == master ws
    where
      master ws =
        case W.integrate' $ W.stack $ W.workspace $ W.current ws of
          [] -> Nothing
          (x:_) -> Just x
  isCommandChecked ToggleSticky w = do
    ws <- gets windowset
    let copies = CW.copiesOfOn (Just w) (CW.taggedWindows $ W.hidden ws)
    return $ not $ null copies
  isCommandChecked _ _ = return False

-- | Generic data type for decoration widgets.
data GenericWidget cmd =
      TitleWidget                      -- ^ Window title (just text label)
    | WindowIcon { swCommand :: !cmd } -- ^ Window icon with some associated command
    -- | Other widgets
    | GenericWidget {
        swCheckedText :: !String       -- ^ Text for checked widget state
      , swUncheckedText :: !String     -- ^ Text for unchecked widget state
      , swCommand :: !cmd              -- ^ Window command
    }
    deriving (Show, Read)

-- | Generic widget type specialized for StandardCommand
type StandardWidget = GenericWidget StandardCommand

instance DecorationWidget (GenericWidget StandardCommand) where

  type WidgetCommand (GenericWidget StandardCommand) = StandardCommand

  widgetCommand TitleWidget _ = FocusWindow
  widgetCommand w 1 = swCommand w
  widgetCommand _ _ = FocusWindow

  isShrinkable TitleWidget = True
  isShrinkable _ = False

-- | Check if the widget should be displayed in `checked' state.
isWidgetChecked :: DecorationWidget widget => widget -> Window -> X Bool
isWidgetChecked wdt = isCommandChecked (widgetCommand wdt 1)

-- | Type class for widgets that can be displayed as
-- text fragments by TextDecoration engine.
class DecorationWidget widget => TextWidget widget where
  widgetString :: DrawData engine widget -> widget -> X String

instance TextWidget StandardWidget where
    widgetString dd TitleWidget = return $ ddWindowTitle dd
    widgetString _ (WindowIcon {}) = return "[*]"
    widgetString dd w = do
      checked <- isWidgetChecked w (ddOrigWindow dd)
      if checked
        then return $ swCheckedText w
        else return $ swUncheckedText w

-- | Widget for window title
titleW :: StandardWidget
titleW = TitleWidget

-- | Widget for ToggleSticky command.
toggleStickyW :: StandardWidget
toggleStickyW = GenericWidget "[S]" "[s]" ToggleSticky

-- | Widget for Minimize command
minimizeW :: StandardWidget
minimizeW = GenericWidget "" "[_]" Minimize

-- | Widget for ToggleMaximize command
maximizeW :: StandardWidget
maximizeW = GenericWidget "" "[O]" ToggleMaximize

-- | Widget for CloseWindow command
closeW :: StandardWidget
closeW = GenericWidget "" "[X]" CloseWindow

dwmpromoteW :: StandardWidget
dwmpromoteW = GenericWidget "[M]" "[m]" DwmPromote

moveToNextGroupW :: StandardWidget
moveToNextGroupW = GenericWidget "" "[>]" MoveToNextGroup

moveToPrevGroupW :: StandardWidget
moveToPrevGroupW = GenericWidget "" "[<]" MoveToPrevGroup

