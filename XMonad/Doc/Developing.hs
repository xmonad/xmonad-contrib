-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc.Developing
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
-- xmonad source code and want an brief overview of it.
--
-- While some knowledge of Haskell is required, still this document is
-- also intended for the beginner\/intermediate Haskell programmer who
-- could find writing an xmonad extension a motivation for deepening
-- her understanding of this powerful functional language.
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
    -- -- * Writing new extensions
    -- -- $writing

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
-}

{- $xmonad-libs

Starting from version 0.5, xmonad and xmonad-contrib are packaged and
distributed as libraries. This way of distributing xmonad has many
advantages, since it allows the packaging by GNU\/Linux distributions
while letting the user have the possibility of greatly customizing the
window manager to fit her needs.

Basically, xmonad and the xmonad-contrib libraries let users write
their own window manager in a matter of a few lines of code.

In fact, what seems to be just a configuration file,
@~\/.xmonad\/xmonad.hs@ (whose presence is not necessary for running
the default configuration), is indeed a full Haskell program, with its
@main@ entry point.

This makes it possible, not only to edit the xmonad default
configuration, as we have seen the "XMonad.Doc.Extending" document,
but also to use the Haskell programming language to extend the tasks
performed by the window manager you are writing every time you write
your own @~\/.xmonad\/xmonad.hs@.

This is obviously out of the scope of this document, which instead
will be focused on the xmonad internals, by describing, very briefly,
the programming techniques that have been employed in order to make
writing new extensions very simple.

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
entry point will be used, which calls 'XMonad.Main.xmonad', from the
"XMonad.Main" module.

So, the real @main@ entry point, the one that even users' application
in @~\/.xmonad\/xmonad.hs@ must call, is 'XMonad.Main.xmonad'

'XMonad.Main.xmonad' takes the configuration as its only argument,
configuration whose type ('XMonad.Core.XConfig') is defined in
"XMonad.Core".

'XMonad.Main.xmonad' takes care of opening the connection with the X
server, initializing the state (or deserializing it when restarted)
and the configuration, and calling the event handler
('XMonad.Main.handle') that will 'Prelude.forever' loop waiting for
events and acting accordingly.

-}

{- $internalState

The event loop which calls 'XMonad.Main.handle' to react to events is
run within the 'XMonad.Core.X' monad, which is a
'Control.Monad.State.StateT' transformer over 'IO', encapsulated
within a 'Control.Monad.Reader.ReaderT' transformer. The
'Control.Monad.State.StateT' transformer encapsulates the (writable)
state of the window manager ('XMonad.Core.XState'), whereas the
'Control.Monad.Reader.ReaderT' transformer encapsulates the
(read-only) configuration ('XMonad.Core.XConf').

Thanks to the @newtype deriving@ clause the instance of the
'Control.Monad.State.MonadState' class parametrized over
'XMonad.Core.XState' and the instance of the
'Control.Monad.Reader.MonadReader' class parametrized over
'XMonad.Core.XConf' are automatically derived from us by ghc. This way
we can use 'Control.Monad.State.get', 'Control.Monad.State.gets' and
'Control.Monad.State.modify' for the 'XMonad.Core.XState', and
'Control.Monad.Reader.ask' and 'Control.Monad.Reader.asks' for
reading the 'XMonad.Core.XConf'.

'XMonad.Core.XState' is where all the sensitive information about
windows managing is stored. And the main record of the
'XMonad.Core.XState' is the 'XMonad.Core.windowset', whose type
('XMonad.Core.WindowSet') is a type synonymous for a
'XMonad.StackSet.StackSet' parametrized over a
'XMonad.Core.WorkspaceID' (a 'String'), a layout type wrapped inside
the 'XMonad.Layout.Layout' existential data type, the
'Graphics.X11.Types.Window' type, the 'XMonad.Core.ScreenID' and the
'XMonad.Core.ScreenDetail's.

What a 'XMonad.StackSet.StackSet' is and how it can be manipulated
with pure functions is perfectly described in the Haddock
documentation of the "XMonad.StackSet" module, and will not be repeated
here.

The 'XMonad.StackSet.StackSet' ('XMonad.Core.WindowSet') has 4
records:

* 'XMonad.StackSet.current', for the current, focused workspace. This
  is a 'XMonad.StackSet.Screen', composed by a
  'XMonad.StackSet.Workspace', and the screen information (for
  Xinerama support).

* 'XMonad.StackSet.visible', a list of 'XMonad.StackSet.Screen's for
  the other visible (with Xinerama) workspaces.

* 'XMonad.StackSet.hidden', the list of 'XMonad.StackSet.Screen's for
  non visible workspaces.

The 'XMonad.StackSet.Workspace' type is made of a
'XMonad.StackSet.tag', a 'XMonad.StackSet.layout' and
'XMonad.StackSet.stack', possibly empy, of windows.

"XMonad.StackSet", to be imported qualified, provides many pure
functions to manipulate the 'XMonad.StackSet.StackSet'. These
functions are usually used as the argument of
'XMonad.Operations.windows', which indeed takes a pure function to
manipulate the 'XMonad.Core.WindowSet' and does all the needed
operations to refresh the screen and save the modified
'XMonad.Core.XState'.

During 'XMonad.Operations.windows' calls the 'XMonad.StackSet.layout'
record of the 'XMonad.StackSet.current' and 'XMonad.StackSet.visible'
'XMonad.StackSet.Workspace's is used to arrange the
'XMonad.StackSet.stack' of windows of each workspace.

The possibility of manipulating the 'XMonad.StackSet.StackSet'
('XMonad.Core.WindowSet') with pure functions makes it possible to
test all the properties of those functions with QuickCheck, providing
greater reliability of the core code.

Every change to the "XMonad.StackSet" module must be accompanied with
the set of property to be tested with QuickCheck before being applied.

-}

{- $events

Events and event handling are the main data and activity xmonad is
involved with. And X Events are one of the most important.

Still there may be events that are generated by layouts, or by the
user, for sending commands to layouts, for instance.

"XMonad.Core" defines a class that generalizes the concept of events,
'XMonad.Core.Message', constrained to types with a
'Data.Typeable.Typeable' instance definition (which can be
automatically derived by ghc).

'XMonad.Core.Message's are wrapped within an existential type
'XMonad.Core.SomeMessage'.

The 'Data.Typeable.Typeable' constraint allows us to define a
'XMonad.Core.fromMessage' function that can unwrap the message with
'Data.Typeable.cast'.

X Events are instances of this class.

By using the 'Data.Typeable.Typeable' class for any kind of
'XMonad.Core.Message's and events we can define polymorphic functions
and use them for processing messages or unhandled events.

This is precisely what happens with X events: xmonad passes them to
'XMonad.Main.handle'. If the main event handling function doesn't have
anything to do with the event, the event is sent to all visible
layouts by 'XMonad.Operations.bradcastMessage'.

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
