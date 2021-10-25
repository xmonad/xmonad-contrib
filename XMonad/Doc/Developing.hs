-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc.Developing
-- Description :  Brief overview of the xmonad internals.
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This module gives a brief overview of the xmonad internals. It is
-- intended for advanced users who are curious about the xmonad source
-- code and want an brief overview. This document may also be helpful
-- for the beginner\/intermediate Haskell programmer who is motivated
-- to write an xmonad extension as a way to deepen her understanding
-- of this powerful functional language; however, there is not space
-- here to go into much detail.  For a more comprehensive document
-- covering some of the same material in more depth, see the guided
-- tour of the xmonad source on the xmonad wiki:
-- <http://haskell.org/haskellwiki/Xmonad/Guided_tour_of_the_xmonad_source>.
--
-- If you write an extension module and think it may be useful for
-- others, consider releasing it.  Coding guidelines and licensing
-- policies are covered at the end of this document, and must be
-- followed if you want your code to be included in the official
-- repositories.  For a basic tutorial on the nuts and bolts of
-- developing a new extension for xmonad, see the tutorial on the
-- wiki:
-- <http://haskell.org/haskellwiki/Xmonad/xmonad_development_tutorial>.
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

Here are some libraries that may be useful when writing your own module:

  - XMonad.Prelude: Re-export commonly used functions from prelude, as
    well as some xmonad-specific helpers.
-}

{- $xmonad-libs

Starting with version 0.5, xmonad and xmonad-contrib are packaged and
distributed as libraries, instead of components which must be compiled
by the user into a binary (as they were prior to version 0.5). This
way of distributing xmonad has many advantages, since it allows
packaging by GNU\/Linux distributions while still allowing the user to
customize the window manager to fit her needs.

Basically, xmonad and the xmonad-contrib libraries let users write
their own window manager in just a few lines of code. While
@~\/.xmonad\/xmonad.hs@ at first seems to be simply a configuration
file, it is actually a complete Haskell program which uses the xmonad
and xmonad-contrib libraries to create a custom window manager.

This makes it possible not only to edit the default xmonad
configuration, as we have seen in the "XMonad.Doc.Extending" document,
but to use the Haskell programming language to extend the window
manager you are writing in any way you see fit.

-}

{- $internals
-}

{- $main
#The_main_entry_point#

xmonad installs a binary, @xmonad@, which must be executed by the
Xsession starting script. This binary, whose code can be read in
@Main.hs@ of the xmonad source tree, will use 'XMonad.Core.recompile'
to run @ghc@ in order to build a binary from @~\/.xmonad\/xmonad.hs@.
If this compilation process fails, for any reason, a default @main@
entry point will be used, which calls the 'XMonad.Main.xmonad'
function with a default configuration.

Thus, the real @main@ entry point, the one that even the users' custom
window manager application in @~\/.xmonad\/xmonad.hs@ must call, is
the 'XMonad.Main.xmonad' function. This function takes a configuration
as its only argument, whose type ('XMonad.Core.XConfig')
is defined in "XMonad.Core".

'XMonad.Main.xmonad' takes care of opening the connection with the X
server, initializing the state (or deserializing it when restarted)
and the configuration, and calling the event handler
('XMonad.Main.handle') that goes into an infinite loop (using
'Prelude.forever') waiting for events and acting accordingly.

-}

{- $internalState

The event loop which calls 'XMonad.Main.handle' to react to events is
run within the 'XMonad.Core.X' monad, which is a
'Control.Monad.State.StateT' transformer over 'IO', encapsulated
within a 'Control.Monad.Reader.ReaderT' transformer. The
'Control.Monad.State.StateT' transformer encapsulates the
(read\/writable) state of the window manager (of type
'XMonad.Core.XState'), whereas the 'Control.Monad.Reader.ReaderT'
transformer encapsulates the (read-only) configuration (of type
'XMonad.Core.XConf').

Thanks to GHC's newtype deriving feature, the instance of the
'Control.Monad.State.MonadState' class parametrized over
'XMonad.Core.XState' and the instance of the
'Control.Monad.Reader.MonadReader' class parametrized over
'XMonad.Core.XConf' are automatically derived for the 'XMonad.Core.X'
monad. This way we can use 'Control.Monad.State.get',
'Control.Monad.State.gets' and 'Control.Monad.State.modify' for the
'XMonad.Core.XState', and 'Control.Monad.Reader.ask' and
'Control.Monad.Reader.asks' for reading the 'XMonad.Core.XConf'.

'XMonad.Core.XState' is where all the sensitive information about
window management is stored. The most important field of the
'XMonad.Core.XState' is the 'XMonad.Core.windowset', whose type
('XMonad.Core.WindowSet') is a synonym for a
'XMonad.StackSet.StackSet' parametrized over a
'XMonad.Core.WorkspaceID' (a 'String'), a layout type wrapped inside
the 'XMonad.Layout.Layout' existential data type, the
'Graphics.X11.Types.Window' type, the 'XMonad.Core.ScreenID' and the
'XMonad.Core.ScreenDetail's.

What a 'XMonad.StackSet.StackSet' is and how it can be manipulated
with pure functions is described in the Haddock documentation of the
"XMonad.StackSet" module.

The 'XMonad.StackSet.StackSet' ('XMonad.Core.WindowSet') has four
fields:

* 'XMonad.StackSet.current', for the current, focused workspace. This
  is a 'XMonad.StackSet.Screen', which is composed of a
  'XMonad.StackSet.Workspace' together with the screen information (for
  Xinerama support).

* 'XMonad.StackSet.visible', a list of 'XMonad.StackSet.Screen's for
  the other visible (with Xinerama) workspaces.  For non-Xinerama
  setups, this list is always empty.

* 'XMonad.StackSet.hidden', the list of non-visible
  'XMonad.StackSet.Workspace's.

* 'XMonad.StackSet.floating', a map from floating
  'Graphics.X11.Types.Window's to 'XMonad.StackSet.RationalRect's
  specifying their geometry.

The 'XMonad.StackSet.Workspace' type is made of a
'XMonad.StackSet.tag', a 'XMonad.StackSet.layout' and
a (possibly empty) 'XMonad.StackSet.stack' of windows.

"XMonad.StackSet" (which should usually be imported qualified, to
avoid name clashes with Prelude functions such as 'Prelude.delete' and
'Prelude.filter') provides many pure functions to manipulate the
'XMonad.StackSet.StackSet'. These functions are most commonly used as
an argument to 'XMonad.Operations.windows', which takes a pure
function to manipulate the 'XMonad.Core.WindowSet' and does all the
needed operations to refresh the screen and save the modified
'XMonad.Core.XState'.

During each 'XMonad.Operations.windows' call, the
'XMonad.StackSet.layout' field of the 'XMonad.StackSet.current' and
'XMonad.StackSet.visible' 'XMonad.StackSet.Workspace's are used to
physically arrange the 'XMonad.StackSet.stack' of windows on each
workspace.

The possibility of manipulating the 'XMonad.StackSet.StackSet'
('XMonad.Core.WindowSet') with pure functions makes it possible to
test all the properties of those functions with QuickCheck, providing
greater reliability of the core code. Every change to the
"XMonad.StackSet" module must be accompanied by appropriate QuickCheck
properties before being applied.

-}

{- $events

Event handling is the core activity of xmonad.  Events generated by
the X server are most important, but there may also be events
generated by layouts or the user.

"XMonad.Core" defines a class that generalizes the concept of events,
'XMonad.Core.Message', constrained to types with a
'Data.Typeable.Typeable' instance definition (which can be
automatically derived by GHC). 'XMonad.Core.Message's are wrapped
within an existential type 'XMonad.Core.SomeMessage'. The
'Data.Typeable.Typeable' constraint allows for the definition of a
'XMonad.Core.fromMessage' function that can unwrap the message with
'Data.Typeable.cast'.  X Events are instances of this class, along
with any messages used by xmonad itself or by extension modules.

Using the 'Data.Typeable.Typeable' class for any kind of
'XMonad.Core.Message's and events allows us to define polymorphic functions
for processing messages or unhandled events.

This is precisely what happens with X events: xmonad passes them to
'XMonad.Main.handle'. If the main event handling function doesn't have
anything to do with the event, the event is sent to all visible
layouts by 'XMonad.Operations.broadcastMessage'.

This messaging system allows the user to create new message types,
simply declare an instance of the 'Data.Typeable.Typeable' and use
'XMonad.Operations.sendMessage' to send commands to layouts.

And, finally, layouts may handle X events and other messages within the
same function... miracles of polymorphism.

-}

{- $layoutClass
#The_LayoutClass#
to do
-}

{- $style

For coding style guidelines while contributing, please see the
<https://github.com/xmonad/xmonad/blob/master/CONTRIBUTING.md#style-guidelines style guidelines>
of xmonad's CONTRIBUTING.md.

For examples of Haddock documentation syntax, have a look at
<https://haskell-haddock.readthedocs.io/en/latest/markup.html its documentation>
or other extensions.  Important points are:

* Every exported function (or even better, every function) should have
  a Haddock comment explaining what it does, and providing examples.

* Literal chunks of code can be written in comments using
  \"birdtrack\" notation (a greater-than symbol at the beginning of
  each line).  Be sure to leave a blank line before and after each
  birdtrack-quoted section.

* Link to functions by surrounding the names in single quotes, modules
  in double quotes.

* Literal quote marks and slashes should be escaped with a backslash.

To generate and view the Haddock documentation for your extension, run

> stack haddock --no-haddock-deps

If the builds succeeds, at the end stack should tell you where the
generated @index.html@ is located.

Alternatively, you can also run

> cabal haddock

to similar effect.

For more information, see the Haddock documentation:
<http://www.haskell.org/haddock/doc/html/index.html>.

For more information on the nuts and bolts of how to develop your own
extension, see the tutorial on the wiki:
<http://haskell.org/haskellwiki/Xmonad/xmonad_development_tutorial>.

-}

{- $license

New modules should identify the author, and be submitted under the
same license as xmonad (BSD3).

-}
