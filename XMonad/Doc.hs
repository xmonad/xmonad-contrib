-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This is the main documentation module for the xmonad-contrib
-- library.
--
-- The module provides a brief overview of xmonad and a link to the
-- documentation for configuring and extending your xmonad window
-- manager.
--
-- A link to the module describing xmonad internals is also provided.
-- This module is mainly dedicated to those wanting to contribute code
-- for this library and for the curious ones, who want to know what's
-- going on behind the scene.
-----------------------------------------------------------------------------

module XMonad.Doc
    (
    -- * Overview
    -- $overview

    -- * Configuring xmonad
    -- $configuring
    module XMonad.Doc.Configuring,

    -- * Extending xmonad with the xmonad-contrib library
    -- $extending
    module XMonad.Doc.Extending,

    -- * Developing xmonad: an brief code commentary
    -- $developing
    module XMonad.Doc.Developing

    ) where


import XMonad.Doc.Configuring
import XMonad.Doc.Extending
import XMonad.Doc.Developing

--------------------------------------------------------------------------------
--
--  Overview
--
--------------------------------------------------------------------------------

{- $overview
#Overview#

xmonad is a tiling window manager for X. This library collects third
party tiling algorithms, hooks, configurations and scripts to xmonad.
The source for this library is available via darcs get
<http://code.haskell.org/XMonadContrib>

Each stable release of xmonad comes with a stable release of the
contrib library too, which should be used if you're using the stable
release. You can find the tarball here
(<http://www.xmonad.org/XMonadContrib-0.4.tar.gz>) (Oct 2007)

-}

{- $configuring

This module is dedicated at configuring xmonad. A brief tutorial will
guide you through the basic configuration steps.

-}

{- $extending

This module is dedicated at extending xmonad with the xmonad-contrib
library. You will find an overview of the library and instruction on
installing contributed extensions.

-}

{- $developing

This module consists of a brief description of the xmonad internals.
It is mainly intended for contributors and basically provides a brief
code commentary with link to the source code documentation.

-}