-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.WorkspaceDir
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

import XMonadContrib.Anneal ()
-- commented because of conflicts with 6.6's instances import XMonadContrib.BackCompat ()
import XMonadContrib.Circle ()
-- TODO commented because it requires hs-boot modifications import XMonadContrib.Commands ()
import XMonadContrib.Combo ()
import XMonadContrib.Decoration ()
import XMonadContrib.Dmenu ()
import XMonadContrib.DwmPromote ()
import XMonadContrib.DynamicLog ()
import XMonadContrib.Dzen ()
import XMonadContrib.FindEmptyWorkspace ()
import XMonadContrib.GreedyView ()
import XMonadContrib.HintedTile ()
import XMonadContrib.LayoutHints ()
import XMonadContrib.MagicFocus ()
import XMonadContrib.Mosaic ()
import XMonadContrib.Magnifier ()
import XMonadContrib.NamedWindows ()
import XMonadContrib.NoBorders ()
import XMonadContrib.RotView ()
import XMonadContrib.SimpleDate ()
import XMonadContrib.Spiral ()
import XMonadContrib.Square ()
import XMonadContrib.Submap ()
import XMonadContrib.Tabbed ()
import XMonadContrib.TwoPane ()
import XMonadContrib.Warp ()
import XMonadContrib.WorkspaceDir ()
