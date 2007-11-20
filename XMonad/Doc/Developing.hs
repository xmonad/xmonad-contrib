-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This module documents the xmonad internals.
--
-- It is intended for the advanced users who are curious about the
-- xmonad code and want an brief overview of it.
--
-- It may be useful also for those who would like to extend xmonad. If
-- you think your extension may be useful for other users too, you may
-- consider about releasing it.
--
-- Coding guidelines and licencing policies must be followed if you
-- want your code to be included in the official repositories.
--
-----------------------------------------------------------------------------

module XMonad.Doc.Developing
    (
    -- * Writing new extensions
    -- $writing

    -- * Libraries for writing window managers
    -- $xmonad-libs

    -- * xmonad internals
    -- $internals

    -- ** The @main@ entry point
    -- $main

    -- ** The X monad and the internal state
    -- $internalState

    -- ** Event handling and messages
    -- $events

    -- ** The 'LayoutClass'
    -- $layoutClass

    -- * Coding style
    -- $style

    -- * Licensing policy
    -- $license
    ) where

--------------------------------------------------------------------------------
--
--  Writing Extensions
--
--------------------------------------------------------------------------------

{- $writing

Writing Other Extensions

-}

{- $xmonad-libs

xmonad and xmonad-contrib are just libraries for letting users write
their own window managers. This is what makes xmonad such a powerful
and still simple application.

Give some examples:
arossato_vm
droundy_wm

In the previous sections we show how simple it can be to write your
own window manager by using the core code (xmonad) and some of the
contributed code (xmonad-contrib).

In this section we will give you a brief overview of the programming
techniques that have been used in order to make writing new extensions
very simple.

-}

{- $internals

TODO

-}

{- $main
#The_main_entry_point#

TODO

-}

{- $internalState

TODO

-}

{- $events

TODO

-}

{- $layoutClass
#The_LayoutClass#
TODO

-}

{- $style

These are the coding guidelines for contributing to xmonad and the
xmonad contributed extensions.

* Comment every top level function (particularly exported funtions), and
  provide a type signature.

* Use Haddock syntax in the comments.

* Follow the coding style of the other modules.

* Code should be compilable with -Wall -Werror. There should be no warnings.

* Partial functions should be avoided: the window manager should not
  crash, so do not call 'error' or 'undefined'

* Tabs are /illegal/. Use 4 spaces for indenting.

* Any pure function added to the core should have QuickCheck properties
  precisely defining its behaviour.

-}

{- $license

New modules should identify the author, and be submitted under the
same license as xmonad (BSD3 license or freer).

-}
