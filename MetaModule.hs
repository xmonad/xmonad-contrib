-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.MetaModule
-- Copyright   :  (c) 2007  Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is an artificial dependency on all the XMonad.* modules.  It is
-- intended to help xmonad hackers ensure that contrib modules build after API
-- changes.
--
-- Please add new modules to this list (in alphabetical order).
--
-----------------------------------------------------------------------------


module XMonad.MetaModule () where

import XMonad.Accordion ()
import XMonad.Anneal ()
import XMonad.Circle ()
import XMonad.Commands ()
-- import XMonad.Combo () -- broken under ghc head
import XMonad.ConstrainedResize ()
import XMonad.CopyWindow ()
import XMonad.CycleWS ()
import XMonad.DeManage ()
import XMonad.DirectoryPrompt ()
import XMonad.Dishes ()
import XMonad.Dmenu ()
import XMonad.DragPane ()
import XMonad.DwmPromote ()
import XMonad.DynamicLog ()
import XMonad.DynamicWorkspaces ()
import XMonad.Dzen ()
import XMonad.EwmhDesktops ()
import XMonad.FindEmptyWorkspace ()
import XMonad.FlexibleResize ()
import XMonad.FlexibleManipulate ()
import XMonad.FloatKeys ()
import XMonad.FocusNth ()
import XMonad.Grid ()
import XMonad.Invisible ()
-- import XMonad.HintedTile ()
-- import XMonad.LayoutCombinators ()
import XMonad.LayoutModifier ()
import XMonad.LayoutHints ()
import XMonad.LayoutScreens ()
import XMonad.MagicFocus ()
-- import XMonad.ManageDocks ()
import XMonad.ManPrompt ()
-- import XMonad.Magnifier ()
import XMonad.Maximize ()
-- import XMonad.Mosaic ()
import XMonad.MosaicAlt ()
import XMonad.MouseGestures ()
import XMonad.MultiToggle ()
import XMonad.NamedWindows ()
import XMonad.NoBorders ()
import XMonad.ResizableTile ()
import XMonad.Roledex ()
import XMonad.RotSlaves ()
import XMonad.RotView ()
import XMonad.Run ()
import XMonad.SetWMName ()
import XMonad.ShellPrompt ()
import XMonad.SimpleDate ()
import XMonad.SinkAll ()
import XMonad.Spiral ()
import XMonad.Square ()
import XMonad.SshPrompt ()
import XMonad.Submap ()
import XMonad.SwapWorkspaces ()
-- import XMonad.SwitchTrans ()
import XMonad.Tabbed ()
import XMonad.TagWindows ()
import XMonad.ThreeColumns ()
import XMonad.TilePrime ()
import XMonad.ToggleLayouts ()
import XMonad.TwoPane ()
import XMonad.XMonadPrompt ()
import XMonad.XPrompt ()
import XMonad.XPropManage ()
import XMonad.XSelection ()
import XMonad.XUtils ()
import XMonad.Warp ()
import XMonad.WindowBringer ()
import XMonad.WindowNavigation ()
import XMonad.WindowPrompt ()
import XMonad.WmiiActions ()
import XMonad.WorkspaceDir ()
import XMonad.WorkspacePrompt ()
