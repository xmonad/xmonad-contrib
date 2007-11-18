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
-- This is a module for documenting the xmonad-contrib library.
--
-----------------------------------------------------------------------------

module Documentation
    (
    -- * Configuring xmonad
    -- $configure

    -- ** A simple example
    -- $example

    -- ** Checking whether your xmonad.hs is correct
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

    -- ** Libraries for writing window managers
    -- $xmonad-libs

    -- ** xmonad internals
    -- $internals

    -- *** The @main@ entry point
    -- $main

    -- *** The X monad and the internal state
    -- $internalState

    -- *** Event handling and messages
    -- $events

    -- *** The 'LayoutClass'
    -- $layoutClass

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
#Configuring_xmonad#
xmonad can be configured by creating and editing the Haskell file:

>    ~/.xmonad/xmonad.hs

If this file does not exist, xmonad will simply use default settings;
if it does exist, xmonad will use whatever settings you specify.  Note
that this file can contain arbitrary Haskell code, which means that
you have quite a lot of flexibility in configuring xmonad.

NOTE for users of previous versions (< 0.5) of xmonad: this is a major
change in the way xmonad is configured.  Prior to version 0.5,
configuring xmonad required editing an xmonad source file called
Config.hs, recompiling xmonad, and then restarting.  From version 0.5
onwards, however, all you have to do is edit xmonad.hs and restart
with @mod-q@; xmonad does the recompiling itself.  The format of the
configuration file has also changed; it is now simpler and much
shorter, only requiring you to list those settings which are different
from the defaults.

-}

{- $example
#A_simple_example#

Here is a basic example, which starts with the default xmonad
configuration and overrides the border width, default terminal, and
some colours:

>    --
>    -- An example, simple ~/.xmonad/xmonad.hs file.
>    -- It overrides a few basic settings, reusing all the other defaults.
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

However, note that you should not edit Config.hs itself.

-}

{- $check
#Checking_whether_your_xmonad.hs_is_correct#

After changing your configuration, it is a good idea to check that it
is syntactically and type correct.  You can do this easily by loading
your configuration file in the Haskell interpreter:

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
#Loading_your_configuration#

To get xmonad to use your new settings, type @mod-q@. xmonad will
attempt to compile this file, and run it.  If everything goes well,
xmonad will seamlessly restart itself with the new settings, keeping
all your windows, layouts, etc. intact.  If something goes wrong, the
previous (default) settings will be used.  Note this requires
that GHC and xmonad are in your @$PATH@. If GHC isn't in your
path, you can still compile @xmonad.hs@ yourself:

>    $ cd ~/.xmonad
>    $ /path/to/ghc --make xmonad.hs
>    $ ls
>    xmonad    xmonad.hi xmonad.hs xmonad.o

When you hit @mod-q@, this newly compiled xmonad will be used.

-}

{- $where
#Where_are_the_defaults?#

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

See "Documentation#Editing_key_bindings" for instruction on how to
edit key bindings for adding actions.

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
  "Documentation#Editing_the_manage_hook" for more information on
  customizing the 'XMonad.Core.manageHook'.

* 'XMonad.Core.logHook': this hook is called when the stack of windows
  managed by xmonad has been changed, by calling the
  'XMonad.Operations.windows' function. For instance
  "XMonad.Hooks.DynamicLog" will produce a string (whose format can be
  configured) to be printed to the standard output. This can be used
  to display some information about the xmonad state in a Status Bar.
  See "Documentation#The_log_hook_and_external_status_bars" for more
  information.

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
'XMonad.Core.layoutHook' see "Documentation#Editing_the_layout_hook".

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
"Documentation#Editing_key_bindings" on how to configure xmonad to use
some prompts. The give examples include adding some prompts.

-}

{- $utils

In the @XMonad.Util@ name space you can find modules exporting various
utility functions that are used by the other modules of the
xmonad-contrib library.

There are also utilities for helping in configuring xmonad or using
external utilities.

A non complete list with a brief description:

* "XMonad.Util.CustomKeys" or "XMonad.Util.EZConfig" can be used to
  configure key bindings (see "Documentation#Editing_key_bindings");

* "XMonad.Util.Dzen" "XMonad.Util.Dmenu" provide useful functions for
  running dzen as a xmonad status bar and dmenu as a program launcher;

* "XMonad.Util.XSelection" provide utilities for using the mouse
  selection;

* "XMonad.Util.XUtils" and "XMonad.Util.Font" are libraries for
  accessing Xlib and XFT function in a convenient way.

-}

--------------------------------------------------------------------------------
--
--  Extending Xmonad
--
--------------------------------------------------------------------------------

{- $extending
#Extending_xmonad#

Since the @xmonad.hs@ file is just another Haskell module, you may
import and use any Haskell code or libraries you wish, such as
extensions from the xmonad-contrib library, or other code you write
yourself.

-}

{- $keys
#Editing_key_bindings#

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

Sometimes, more than completely redefining the key bindings, as we did
above, we may want to add some new bindings, or\/and remove existing
ones.

-}

{- $keyAdding
#Adding_key_bindings#

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
#Removing_key_bindings#

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
#Adding_and_removing_key_bindings#

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
#Editing_the_layout_hook#

When you start an application that opens a new window, when you change
the focused window, or move it to another workspace, or change that
workspace's layout, xmonad will use the 'XMonad.Core.layoutHook' for
reordering the visible windows on the visible workspace(s).

Since different layouts may be attached to different workspaces, and
you can change them, xmonad needs to know which one to pick up, so,
the layoutHook may be thought as a stack, or even better a combination
of layouts. This also means an order, i.e. a list.

The problem is that the layout subsystem is implemented with an
advanced feature of the Haskell programming language: type classes.
This allows us to very easily write new layouts, combine or modify
existing layouts, have some of them with a state, etc. See
"Documentation#The_LayoutClass" for more information.

The price we have to pay to get all that for free - which is something
that makes xmonad so powerful with such a ridiculously low number of
lines - is that we cannot simply have a list of layouts as we used to
have before the 0.5 release.

Instead the combination of layouts to be used by xmonad is created
with a specific layout combinator: 'XMonad.Layouts.|||'

Suppose we want a list with the 'XMonad.Layouts.Full', the
'XMonad.Layout.Tabbed.tabbed' and the
'XMonad.Layout.Accordion.Accordion' layouts. First we import, in our
@~\/.xmonad\/xmonad.hs@, all the needed module:

>    import XMonad
>    import XMonad.Layouts
>
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion

Then we create the combination of layouts we need:

>    mylayoutHook = Full ||| tabbed shrinkText defaultTConf ||| Accordion


Now, all we need to do is to change the 'XMonad.Core.layoutHook'
record of the 'XMonad.Core.XConfig' data type, like:

>    main = xmonad defaultConfig { layoutHook = mylayoutHook }

Thanks to the new combinator we can apply a layout modifier to the
combination of layouts, instead of applying it to each one. Suppose we
want to use the 'XMonad.Layout.NoBorders.noBorders' layout modifier,
from the "XMonad.Layout.NoBorders" module (which must be imported):

>    mylayoutHook = noBorders (Full ||| tabbed shrinkText defaultTConf ||| Accordion)

Obviously, if we want only the tabbed layout without borders, then we
may write:

>    mylayoutHook = Full ||| noBorders (tabbed shrinkText defaultTConf) ||| Accordion

The @~\/.xmonad\/xmonad.hs@ will now look like this:

>    import XMonad.Layouts
>    
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion
>    import XMonad.Layout.NoBorders
>    
>    mylayoutHook = Full ||| noBorders (tabbed shrinkText defaultTConf) ||| Accordion
>    
>    main = xmonad defaultConfig { layoutHook = mylayoutHook }

That's it!

-}

{- $manageHook
#Editing_the_manage_hook#
TODO: Manage Hook

-}

{- $logHook
#The_log_hook_and_external_status_bars#

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
