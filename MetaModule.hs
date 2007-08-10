-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.MetaModule
-- Copyright   :  (c) 2007  Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is an artificial dependency on all the XMonadContrib.* modules.  It is
-- intended to help xmonad hackers ensure that contrib modules build after API
-- changes.
--
-- Please add new modules to this list (in alphabetical order).
--
-----------------------------------------------------------------------------


module XMonadContrib.MetaModule () where

import XMonadContrib.Accordion ()
import XMonadContrib.Anneal ()
-- commented because of conflicts with 6.6's instances import XMonadContrib.BackCompat ()
import XMonadContrib.Circle ()
import XMonadContrib.Commands ()
import XMonadContrib.Combo ()
import XMonadContrib.CopyWindow ()
import XMonadContrib.Decoration ()
import XMonadContrib.DeManage ()
import XMonadContrib.Dmenu ()
import XMonadContrib.DwmPromote ()
import XMonadContrib.DynamicLog ()
import XMonadContrib.Dzen ()
import XMonadContrib.FindEmptyWorkspace ()
import XMonadContrib.FlexibleResize ()
import XMonadContrib.FlexibleManipulate ()
import XMonadContrib.FocusNth ()
import XMonadContrib.GreedyView ()
import XMonadContrib.HintedTile ()
import XMonadContrib.LayoutHelpers ()
import XMonadContrib.LayoutHints ()
import XMonadContrib.LayoutHooks ()
import XMonadContrib.LayoutScreens ()
import XMonadContrib.MagicFocus ()
import XMonadContrib.Magnifier ()
import XMonadContrib.Mosaic ()
import XMonadContrib.NamedWindows ()
import XMonadContrib.NoBorders ()
import XMonadContrib.Roledex ()
import XMonadContrib.RotSlaves ()
import XMonadContrib.RotView ()
-- XMonadContrib.ShellPrompt depends on readline
--import XMonadContrib.ShellPrompt ()
import XMonadContrib.SimpleDate ()
import XMonadContrib.SimpleStacking ()
import XMonadContrib.SinkAll ()
import XMonadContrib.Spiral ()
import XMonadContrib.Square ()
import XMonadContrib.SshPrompt ()
import XMonadContrib.Submap ()
import XMonadContrib.SwitchTrans ()
import XMonadContrib.Tabbed ()
import XMonadContrib.ThreeColumns ()
import XMonadContrib.TwoPane ()
import XMonadContrib.ViewPrev ()
import XMonadContrib.XMonadPrompt ()
import XMonadContrib.XPrompt ()
import XMonadContrib.Warp ()
import XMonadContrib.WorkspaceDir ()
