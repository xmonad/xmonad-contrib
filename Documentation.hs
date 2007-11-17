-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a module for documenting the xmonad-contrib library
--
-----------------------------------------------------------------------------

module Documentation
    (
    -- * Configuring xmonad
    -- $configure

    -- ** A simple example
    -- $example

    -- ** Checking your xmonad.hs is correct
    -- $check

    -- ** Loading your configuration
    -- $load

    -- ** Where are the defaults?
    -- $where

    -- * The xmonad-contrib library
    -- $library

    -- ** Actions
    -- $actions

    -- ** Configurations
    -- $configs

    -- ** Hooks
    -- $hooks

    -- ** Layouts
    -- $layouts

    -- ** Prompts
    -- $prompts

    -- ** Utilities
    -- $utils

    -- * Extending xmonad
    -- $extending

    -- ** Editing key bindings
    -- $keys

    -- *** Adding key bindings
    -- $keyAdding

    -- *** Removing key bindings
    -- $keyDel

    -- *** Adding and removing key bindings
    -- $keyAddDel

    -- ** Editing the layout hook
    -- $layoutHook

    -- ** Editing the manage hook
    -- $manageHook

    -- ** The log hook and external status bars
    -- $logHook

    -- * Writing new extensions
    -- $writing

    -- ** xmonad internals
    -- $internals

    -- *** The 'LayoutClass'
    -- $layoutClass

    -- *** The X monad and the internal state
    -- $internalState

    -- *** Event handling and messages
    -- $events

    -- ** Coding style
    -- $style

    -- ** Licensing policy
    -- $license
    ) where


--------------------------------------------------------------------------------
--
--  Configuring Xmonad
--
--------------------------------------------------------------------------------

{- $configure

xmonad is configured by creating and editing the Haskell file:

>    ~/.xmonad/xmonad.hs

xmonad then uses default settings from this file as arguments to the
window manager.

-}

{- $example

Here is a basic example, which takes defaults from xmonad, and
overrides the border width, default terminal, and some colours:

>    --
>    -- An example, simple ~/.xmonad/xmonad.hs file.
>    -- It overrides a few basic settings, reusing all the other defaults,
>    --
>
>    import XMonad
>
>    main = xmonad $ defaultConfig
>        { borderWidth        = 2
>        , terminal           = "urxvt"
>        , normalBorderColor  = "#cccccc"
>        , focusedBorderColor = "#cd8b00" }

This will run \'xmonad\', the window manager, with your settings
passed as arguments.

Overriding default settings like this (using \"record update
syntax\"), will yield the shortest config file, as you only have to
describe values that differ from the defaults.

An alternative is to inline the entire default config file from
xmonad, and edit values you wish to change. This is requires more
work, but some users may find this easier. You can find the defaults
in the file:

>    XMonad/Config.hs

-}

{- $check

Place this text in @~\/.xmonad\/xmonad.hs@, and then check that it is
syntactically and type correct, by loading it in the Haskell
interpreter:

>    $ ghci ~/.xmonad/xmonad.hs
>    GHCi, version 6.8.1: http://www.haskell.org/ghc/  :? for help
>    Loading package base ... linking ... done.
>    Ok, modules loaded: Main.
>
>    Prelude Main> :t main
>    main :: IO ()

Ok, looks good.

-}

{- $load

To have xmonad start using your settings, try @mod-q@. xmonad will
attempt to compile this file, and run it. If it is unable to, the
defaults are used. This requires GHC and xmonad are in your @$PATH@
settings. If GHC isn't in your path, you can still compile the
@xmonad.hs@ file yourself:

>    $ cd ~/.xmonad
>    $ ghc --make xmonad.hs
>    $ ls
>    xmonad    xmonad.hi xmonad.hs xmonad.o

When you hit @mod-q@, this newly compiled xmonad will be used.

-}

{- $where

The default configuration values are defined in the source file:

>    XMonad/Config.hs

the 'XMonad.Core.XConfig' data structure itself is defined in:

>     XMonad/Core.hs

See "XMonad.Core".

-}

--------------------------------------------------------------------------------
--
--  The XmonadContrib Library
--
--------------------------------------------------------------------------------

{- $library

The xmonad-contrib (xmc) library is a set of modules contributed by
xmonad hackers and users. Examples include an ion3-like tabbed layout,
a prompt\/program launcher, and various other useful modules.

Some of these modules provide libraries and other useful functions to
write other modules and extensions.

This is a short overview of the xmc content.

-}

{- $actions

In the @XMonad.Actions@ name space you can find modules exporting
functions that can be usually attached to, and thus called with, some
key bindings.

Each module should come with extensive documentation.

There are many examples. Just to name two of them:

* "XMonad.Actions.CycleWS" provides functions to switch to the next or
   the previous workspace ('XMonad.Actions.CycleWS.nextWS' and
   'XMonad.Actions.CycleWS.prevWS', or to move the focused window to
   the next of previous workspace
   ('XMonad.Actions.CycleWS.shiftToNext' and
   'XMonad.Actions.CycleWS.shiftToPrev')

* "XMonad.Actions.DeManage" provides an a method to cease management
  of a window, without unmapping it
  ('XMonad.Actions.DeManage.demanage')

See "Documentation#keys" for instruction on how to edit key bindings
for adding actions.

-}

{- $configs

In the @XMonad.Config@ name space you can find modules exporting the
default configuration of some of the xmonad and xmonad-contrig
libraries developers.

You can use the source code of these configuration examples also as
starting points for writing your own personal configuration.

-}

{- $hooks

In the @XMonad.Hooks@ name space you can find modules exporting hooks.

Hooks are actions that xmonad performs when some events occur. The two
most important hooks are:

* 'XMonad.Core.manageHook': this hook is called when a new window
  xmonad must take care of is created. This is a very powerful hook,
  since it let us look at the new window's properties and act
  accordingly. For instance, we can configure xmonad to put windows
  belonging to a given application in the float layer, not to manage
  dock applications, or open them in a given workspace. See
  "Documentation#manageHook" for more information on customizing the
  'XMonad.Core.manageHook'.

* 'XMonad.Core.logHook': this hook is called when the stack of windows
  managed by xmonad has been changed, by calling the
  'XMonad.Operations.windows' function. For instance
  "XMonad.Hooks.DynamicLog" will produce a string (whose format can be
  configured) to be printed to the standard output. This can be used
  to display some information about the xmonad state in a Status Bar.
  See "Documentation#StatusBar" for more information.

-}

{- $layouts

In the @XMonad.Layout@ name space you can find modules exporting
contributed tiling algorithms, such as a tabbed layout, a circle and a
three columns ones, etc.

Other modules provide facilities for combining different layouts, such
as "XMonad.Layout.Combo", or a complete set of layout combinators,
like "XMonad.Layout.LayoutCombinators"

Layouts can be also modified with layout modifiers. A general
interface for writing layout modifiers is implemented in
"XMonad.Layout.LayoutModifier".

For more information on using those modules for customizing your
'XMonad.Core.layoutHook' see "Documentation#layout".

-}

{- $prompts

In the @XMonad.Prompt@ name space you can find modules exporting
graphical prompts for getting user input and performing, with it,
different actions.

"XMonad.Prompt" provides a library for easily writing prompts.

These are the available prompts:

* "XMonad.Prompt.Directory"

* "XMonad.Prompt.Layout"

* "XMonad.Prompt.Man"

* "XMonad.Prompt.Shell"

* "XMonad.Prompt.Ssh"

* "XMonad.Prompt.Window"

* "XMonad.Prompt.Workspace"

* "XMonad.Prompt.XMonad"

Usually a prompt is called by some key binding. See
"Documentation#keys" on how to configure xmonad to use some prompts.
The give examples include adding some prompts.

-}

{- $utils

In the @XMonad.Util@ name space you can find modules exporting various
utility functions that are used by the othe modules of the
xmonad-contrib library.

-}

--------------------------------------------------------------------------------
--
--  Extending Xmonad
--
--------------------------------------------------------------------------------

{- $extending

Since the @xmonad.hs@ file is just another Haskell module, you may
import and use any Haskell code or libraries you wish, such as
extensions from the xmonad-contrib library, or other code you write
yourself.

-}

{- $keys
#keys#
Editing key bindings means changing the 'XMonad.Core.XConfig.keys'
record of the 'XMonad.Core.XConfig' data type, like:

>    main = xmonad defaultConfig { keys = myKeys }

and providing a proper definition of @myKeys@ such as:

>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]

Remember that this definition requires importing "Graphics.X11.Xlib",
"XMonad.Prompt", "XMonad.Prompt.Shell", and "XMonad.Prompt.XMonad"

Sometimes, more than complitely redifining the key bindings, as we did
above, we may want to add some new bindings, or\/and remove existing
ones.

-}

{- $keyAdding

Adding key bindings can be done in different ways. The type signature
of "XMonad.Core.XConfig.keys" is:

>    keys :: XConfig Layout -> M.Map (ButtonMask,KeySym) (X ())

which means thatm in order to add new bindings you need to create a
'Data.Map.Map' from the list of your new key bindings, you can do that
with 'Data.Map.fromList', and then join this newly created map with
the one of the existing bindings. This can be done with
'Data.Map.union'.

For instance, if you have defined some additional key bindings like
these:

>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]

then you create a new key bindings map by joining the default one with
yours:

>    newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

Finally you need to update accordingly the default configuration
'XMonad.Core.XConfig.keys' record:

>    main = xmonad defaultConfig { keys = newKeys }


And that's it.

At the end your @~\/.xmonad\/xmonad.hs@ would look like this:


>    module Main (main) where
>
>    import XMonad
>
>    import qualified Data.Map as M
>    import Graphics.X11.Xlib
>    import XMonad.Prompt
>    import XMonad.Prompt.Shell
>    import XMonad.Prompt.XMonad
>
>    main :: IO ()
>    main = xmonad defaultConfig { keys = newKeys }
>
>    newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
>
>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]


Obviously there are other ways of defining @newKeys@. For instance,
you could define it like this:

>    newKeys x = foldr (uncurry M.insert) (keys defaultConfig x) (myKeys x)

An even simpler way to add new key bindings is the use of some of the
utilities provided by the xmonad-contrib library. For instance,
"XMonad.Util.EZConfig" and "XMonad.Util.CustomKeys" both provide
useful functions for editing your key bindings. Look, for instance, at
'XMonad.Util.EZConfig.additionalKeys'.

 -}

{- $keyDel

Removing key bindings requires modifying the binding 'Data.Map.Map'.
This can be done with 'Data.Map.difference' or with 'Data.Map.delete'.

Suppose you wan to get rid of @mod-q@ and @mod-shift-q@. To do this
you just need to define a @newKeys@ as a 'Data.Map.difference' between
the default map and the map of the key bindings you want to remove.

>    newKeys x = M.difference (keys defaultConfig x) (M.fromList $ keysToRemove x)
>
>    keysToRemove :: XConfig Layout ->    [((KeyMask, KeySym),X ())]
>    keysToRemove x =
>             [ ((modMask x              , xK_q ), return ())
>             , ((modMask x .|. shiftMask, xK_q ), return ())
>             ]

As you may see we do not need to define an action for the key bindings
we want to get rid of. We just build a map of keys to remove.

It is also possible to define a list of key bindings and then use
'Data.Map.delete' to remove them from the default key bindings, in
which case we should write something like:

>    newKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
>
>    keysToRemove :: XConfig Layout -> [(KeyMask, KeySym)]
>    keysToRemove x =
>             [ (modMask x              , xK_q )
>             , (modMask x .|. shiftMask, xK_q )
>             ]

Another even simpler possibility is the use of some of the utilities
provided by the xmonad-contrib library. Look, for instance, at
'XMonad.Util.EZConfig.removeKeys'.

-}

{- $keyAddDel

Adding and removing key bindings requires to compose the action of
removing and, after that, the action of adding.

This is an example you may find in "XMonad.Config.Arossato":


>    defKeys    = keys defaultConfig
>    delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
>    newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
>    -- remove some of the default key bindings
>    toRemove x =
>        [ (modMask x              , xK_j     )
>        , (modMask x              , xK_k     )
>        , (modMask x              , xK_p     )
>        , (modMask x .|. shiftMask, xK_p     )
>        , (modMask x .|. shiftMask, xK_q     )
>        , (modMask x              , xK_q     )
>        ] ++
>        -- I want modMask .|. shiftMask 1-9 to be free!
>        [(shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]]
>    -- These are my personal key bindings
>    toAdd x   =
>        [ ((modMask x              , xK_F12   ), xmonadPrompt defaultXPConfig )
>        , ((modMask x              , xK_F3    ), shellPrompt  defaultXPConfig )
>        ] ++
>        -- Use modMask .|. shiftMask .|. controlMask 1-9 instead
>        [( (m .|. modMask x, k), windows $ f i)
>         | (i, k) <- zip (workspaces x) [xK_1 .. xK_9]
>        ,  (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]
>        ]

You can achieve the same result by using "XMonad.Util.CustomKeys" and,
specifically, 'XMonad.Util.CustomKeys.customKeys'.

-}

{- $layoutHook
#layout#
TODO: Layouts

-}

{- $manageHook
#manageHook#
TODO: Manage Hook

-}

{- $logHook
#StatusBar#

TODO: Log Hook

-}

--------------------------------------------------------------------------------
--
--  Writing Extensions
--
--------------------------------------------------------------------------------

{- $writing

Writing Other Extensions

-}

{- $internals

TODO

-}


{- $layoutClass

TODO

-}

{- $internalState

TODO

-}

{- $events

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
