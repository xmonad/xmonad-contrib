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
-- library. It provides a brief overview of xmonad and a link to
-- documentation for configuring and extending xmonad.
--
-- A link to documentation describing xmonad internals is also provided.
-- This module is mainly intended for those wanting to contribute code,
-- or for those who are curious to know what's going on behind the scenes.
-----------------------------------------------------------------------------

module XMonad.Doc
    (
    -- * Overview
    -- $overview

    -- * Configuring xmonad
    -- $configuring

    -- * Extending xmonad with the xmonad-contrib library
    -- $extending

    -- * Developing xmonad: a brief code commentary
    -- $developing

    ) where

import XMonad.Doc.Configuring ()
import XMonad.Doc.Extending ()
import XMonad.Doc.Developing ()

--------------------------------------------------------------------------------
--
--  Overview
--
--------------------------------------------------------------------------------

{- $overview
#Overview#

xmonad is a tiling window manager for X. The xmonad-contrib library
collects third party tiling algorithms, hooks, configurations,
scripts, and other extensions to xmonad.  The source for this library
is available from <https://github.com/xmonad/xmonad-contrib> via git:

> git clone https://github.com/xmonad/xmonad-contrib.git

Each stable release of xmonad is accompanied by a stable release of
the contrib library, which you should use if (and only if) you're
using a stable release of xmonad.  You can find the most recent
tarball here:
<http://hackage.haskell.org/cgi-bin/hackage-scripts/package/xmonad-contrib>

-}

{- $configuring

"XMonad.Doc.Configuring" documents the process of configuring
xmonad. A brief tutorial will guide you through the basic
configuration steps.

-}

{- $extending

"XMonad.Doc.Extending" is dedicated to the xmonad-contrib library
itself. You will find an overview of extensions available in the
library and instructions for using them.

-}

{- $developing

"XMonad.Doc.Developing" consists of a brief description of the xmonad
internals.  It is mainly intended for contributors and provides a
brief code commentary with links to the source documentation.

-}
