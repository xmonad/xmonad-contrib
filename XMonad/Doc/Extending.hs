-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc.Extending
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This module documents the xmonad-contrib library and
-- how to use it to extend the capabilities of xmonad.
--
-- Reading this document should not require a deep knowledge of
-- Haskell; the examples are intended to be useful and understandable
-- for those users who do not know Haskell and don't want to have to
-- learn it just to configure xmonad.  You should be able to get by
-- just fine by ignoring anything you don't understand and using the
-- provided examples as templates.  However, relevant Haskell features
-- are discussed when appropriate, so this document will hopefully be
-- useful for more advanced Haskell users as well.
--
-- Those wishing to be totally hardcore and develop their own xmonad
-- extensions (it's easier than it sounds, we promise!) should read
-- the documentation in "XMonad.Doc.Developing".
--
-- More configuration examples may be found on the Haskell wiki:
--
-- <http://haskell.org/haskellwiki/Xmonad/Config_archive>
--
-----------------------------------------------------------------------------

module XMonad.Doc.Extending
    (
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
    ) where

--------------------------------------------------------------------------------
--
--  The XmonadContrib Library
--
--------------------------------------------------------------------------------

{- $library

The xmonad-contrib (xmc) library is a set of extension modules
contributed by xmonad hackers and users, which provide additional
xmonad features.  Examples include various layout modes (tabbed,
spiral, three-column...), prompts, program launchers, the ability to
manipulate windows and workspaces in various ways, alternate
navigation modes, and much more.  There are also \"meta-modules\"
which make it easier to write new modules and extensions.

This is a concise yet complete overview of the xmonad-contrib modules.
For more information about any particular module, just click on its
name to view its Haddock documentation; each module should come with
extensive documentation.  If you find a module that could be better
documented, or has incorrect documentation, please report it as a bug
(<http://code.google.com/p/xmonad/issues/list>)!

-}

{- $actions

In the @XMonad.Actions@ namespace you can find modules exporting
various functions that are usually intended to be bound to key
combinations or mouse actions, in order to provide functionality
beyond the standard keybindings provided by xmonad.

See "XMonad.Doc.Extending#Editing_key_bindings" for instructions on how to
edit your key bindings.

* "XMonad.Actions.Commands": running internal xmonad actions
  interactively.

* "XMonad.Actions.ConstrainedResize": an aspect-ratio-constrained
  window resizing mode.

* "XMonad.Actions.CopyWindow": duplicating windows on multiple
  workspaces.

* "XMonad.Actions.CycleWS": move between workspaces.

* "XMonad.Actions.DeManage": cease management of a window without
  unmapping it.

* "XMonad.Actions.DwmPromote": dwm-like master window swapping.

* "XMonad.Actions.DynamicWorkspaces": add and delete workspaces.

* "XMonad.Actions.FindEmptyWorkspace": find an empty workspace.

* "XMonad.Actions.FlexibleManipulate": move\/resize windows without
  warping the mouse.

* "XMonad.Actions.FlexibleResize": resize windows from any corner.

* "XMonad.Actions.FloatKeys": move\/resize floating windows with
  keybindings.

* "XMonad.Actions.FocusNth": focus the nth window on the screen.

* "XMonad.Actions.MouseGestures": bind mouse gestures to actions.

* "XMonad.Actions.RotSlaves": rotate non-master windows.

* "XMonad.Actions.RotView": cycle through non-empty workspaces.

* "XMonad.Actions.SimpleDate": display the date in a popup menu.

* "XMonad.Actions.SinkAll": sink all floating windows.

* "XMonad.Actions.Submap": create key submaps, i.e. the ability to
  bind actions to key sequences rather than being limited to single
  key combinations.

* "XMonad.Actions.SwapWorkspaces": swap workspace tags.

* "XMonad.Actions.TagWindows": tag windows and select by tag.

* "XMonad.Actions.Warp": warp the pointer.

* "XMonad.Actions.WindowBringer": bring windows to you, and you to
  windows.

* "XMonad.Actions.WmiiActions": wmii-style actions.

-}

{- $configs

In the @XMonad.Config@ namespace you can find modules exporting the
configurations used by some of the xmonad and xmonad-contrib
developers.  You can look at them for examples while creating your own
configuration; you can also simply import them and use them as your
own configuration, possibly with some modifications.

* "XMonad.Config.Arossato"

* "XMonad.Config.Dons"

* "XMonad.Config.Droundy"

* "XMonad.Config.Sjanssen"

-}

{- $hooks

In the @XMonad.Hooks@ namespace you can find modules exporting
hooks. Hooks are actions that xmonad performs when certain events
occur. The two most important hooks are:

* 'XMonad.Core.manageHook': this hook is called when a new window
  xmonad must take care of is created. This is a very powerful hook,
  since it lets us examine the new window's properties and act
  accordingly. For instance, we can configure xmonad to put windows
  belonging to a given application in the float layer, not to manage
  dock applications, or open them in a given workspace. See
  "XMonad.Doc.Extending#Editing_the_manage_hook" for more information on
  customizing 'XMonad.Core.manageHook'.

* 'XMonad.Core.logHook': this hook is called when the stack of windows
  managed by xmonad has been changed, by calling the
  'XMonad.Operations.windows' function. For instance
  "XMonad.Hooks.DynamicLog" will produce a string (whose format can be
  configured) to be printed to the standard output. This can be used
  to display some information about the xmonad state in a status bar.
  See "XMonad.Doc.Extending#The_log_hook_and_external_status_bars" for more
  information.

Here is a list of the modules found in @XMonad.Hooks@:

* "XMonad.Hooks.DynamicLog": for use with 'XMonad.Core.logHook'; send
  information about xmonad's state to standard output, suitable for
  putting in a status bar of some sort. See
  "XMonad.Doc.Extending#The_log_hook_and_external_status_bars".

* "XMonad.Hooks.EwmhDesktops": support for pagers in panel applications.

* "XMonad.Hooks.ManageDocks": handle DOCK and STRUT windows appropriately.

* "XMonad.Hooks.SetWMName": set the WM name.  Useful when e.g. running
  Java GUI programs.

* "XMonad.Hooks.UrgencyHook": configure an action to occur when a window
  sets the urgent flag.

* "XMonad.Hooks.XPropManage": match on XProperties in your
  'XMonad.Core.manageHook'.

-}

{- $layouts

In the @XMonad.Layout@ namespace you can find modules exporting
contributed tiling algorithms, such as a tabbed layout, a circle, a spiral,
three columns, and so on.

You will also find modules which provide facilities for combining
different layouts, such as "XMonad.Layout.Combo", or
"XMonad.Layout.LayoutCombinators".

Layouts can be also modified with layout modifiers. A general
interface for writing layout modifiers is implemented in
"XMonad.Layout.LayoutModifier".

For more information on using those modules for customizing your
'XMonad.Core.layoutHook' see "XMonad.Doc.Extending#Editing_the_layout_hook".

* "XMonad.Layout.Accordion": put non-focused windows in ribbons at the
  top and bottom of the screen.

* "XMonad.Layout.Circle": an elliptical, overlapping layout.

* "XMonad.Layout.Combo": combine multiple layouts into one.

* "XMonad.Layout.Dishes": stack extra windows underneath the master windows.

* "XMonad.Layout.DragPane": split the screen into two windows with a
  draggable divider.

* "XMonad.Layout.Grid": put windows in a square grid.

* "XMonad.Layout.LayoutCombinators": general layout combining.

* "XMonad.Layout.LayoutHints": make layouts respect window size hints.

* "XMonad.Layout.LayoutModifier": a general framework for creating
  layout \"modifiers\"; useful for creating new layout modules.

* "XMonad.Layout.LayoutScreens": divide the screen into multiple
  virtual \"screens\".

* "XMonad.Layout.MagicFocus": automagically put the focused window in
  the master area.

* "XMonad.Layout.Maximize": temporarily maximize the focused window.

* "XMonad.Layout.MosaicAlt": give each window a specified relative
  amount of screen space.

* "XMonad.Layout.MultiToggle": dynamically apply and unapply layout
  transformers.

* "XMonad.Layout.Named": change the names of layouts (as reported by
  e.g. "XMonad.Hooks.DynamicLog").

* "XMonad.Layout.NoBorders": display windows without borders.

* "XMonad.Layout.PerWorkspace": configure layouts on a per-workspace basis.

* "XMonad.Layout.ResizableTile": tiled layout allowing you to change
  width and height of windows.

* "XMonad.Layout.Roledex": a \"completely pointless layout which acts
  like Microsoft's Flip 3D\".

* "XMonad.Layout.Spiral": Fibonacci spiral layout.

* "XMonad.Layout.Square": split the screen into a square area plus the rest.

* "XMonad.Layout.Tabbed": a tabbed layout.

* "XMonad.Layout.ThreeColumns": a layout with three columns instead of two.

* "XMonad.Layout.TilePrime": fill gaps created by resize hints.

* "XMonad.Layout.ToggleLayouts": toggle between two layouts.

* "XMonad.Layout.TwoPane": split the screen horizontally and show two
  windows.

* "XMonad.Layout.WindowNavigation": navigate around a workspace
  directionally instead of using mod-j\/k.

* "XMonad.Layout.WorkspaceDir": set the current working directory in a
  workspace.

-}

{- $prompts

In the @XMonad.Prompt@ name space you can find modules providing
graphical prompts for getting user input and using it to perform
various actions.

The "XMonad.Prompt" provides a library for easily writing new prompt
modules.

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
"XMonad.Doc.Extending#Editing_key_bindings", which includes examples
of adding some prompts.

-}

{- $utils

In the @XMonad.Util@ namespace you can find modules exporting various
utility functions that are used by the other modules of the
xmonad-contrib library.

There are also utilities for helping in configuring xmonad or using
external utilities.

A non complete list with a brief description:

* "XMonad.Util.CustomKeys" or "XMonad.Util.EZConfig" can be used to
  configure key bindings (see "XMonad.Doc.Extending#Editing_key_bindings");

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
field of the 'XMonad.Core.XConfig' record used by xmonad.  For
example, you could write:

>    main = xmonad $ defaultConfig { keys = myKeys }

and provide an appropriate definition of @myKeys@, such as:

>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]

This particular definition also requires importing "Graphics.X11.Xlib"
(for the symbols such as @xK_F12@), "XMonad.Prompt",
"XMonad.Prompt.Shell", and "XMonad.Prompt.XMonad":

> import Graphics.X11.Xlib
> import ...  -- and so on

Usually, rather than completely redefining the key bindings, as we did
above, we want to simply add some new bindings and\/or remove existing
ones.

-}

{- $keyAdding
#Adding_key_bindings#

Adding key bindings can be done in different ways. The type signature
of 'XMonad.Core.XConfig.keys' is:

>    keys :: XConfig Layout -> M.Map (ButtonMask,KeySym) (X ())

In order to add new key bindings, you need to first create an
appropriate 'Data.Map.Map' from a list of key bindings using
'Data.Map.fromList'.  This 'Data.Map.Map' of new key bindings then
needs to be joined to a 'Data.Map.Map' of existing bindings using
'Data.Map.union'.

For instance, if you have defined some additional key bindings like
these:

>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]

then you can create a new key bindings map by joining the default one
with yours:

>    newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

Finally, you can use @newKeys@ in the 'XMonad.Core.XConfig.keys' field
of the configuration:

>    main = xmonad $ defaultConfig { keys = newKeys }

All together, your @~\/.xmonad\/xmonad.hs@ would now look like this:


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
>    main = xmonad $ defaultConfig { keys = newKeys }
>
>    newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
>
>    myKeys x =
>             [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>             , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>             ]


There are other ways of defining @newKeys@; for instance,
you could define it like this:

>    newKeys x = foldr (uncurry M.insert) (keys defaultConfig x) (myKeys x)

However, the simplest way to add new key bindings is to use some
utilities provided by the xmonad-contrib library.  For instance,
"XMonad.Util.EZConfig" and "XMonad.Util.CustomKeys" both provide
useful functions for editing your key bindings.  Look, for instance, at
'XMonad.Util.EZConfig.additionalKeys'.

 -}

{- $keyDel
#Removing_key_bindings#

Removing key bindings requires modifying the 'Data.Map.Map' which
stores the key bindings.  This can be done with 'Data.Map.difference'
or with 'Data.Map.delete'.

For example, suppose you want to get rid of @mod-q@ and @mod-shift-q@
(you just want to leave xmonad running forever). To do this you need
to define @newKeys@ as a 'Data.Map.difference' between the default
map and the map of the key bindings you want to remove.  Like so:

>    newKeys x = M.difference (keys defaultConfig x) (M.fromList $ keysToRemove x)
>
>    keysToRemove :: XConfig Layout ->    [((KeyMask, KeySym),X ())]
>    keysToRemove x =
>             [ ((modMask x              , xK_q ), return ())
>             , ((modMask x .|. shiftMask, xK_q ), return ())
>             ]

As you can see, it doesn't matter what actions we associate with the
keys listed in @keysToRemove@, so we just use @return ()@ (the
\"null\" action).

It is also possible to simply define a list of keys we want to unbind
and then use 'Data.Map.delete' to remove them. In that case we would
write something like:

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

Adding and removing key bindings requires simply combining the steps
for removing and adding.  Here is an example from
"XMonad.Config.Arossato":

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

You can achieve the same result using the "XMonad.Util.CustomKeys"
module; take a look at the 'XMonad.Util.CustomKeys.customKeys'
function in particular.

-}

{- $layoutHook
#Editing_the_layout_hook#

When you start an application that opens a new window, when you change
the focused window, or move it to another workspace, or change that
workspace's layout, xmonad will use the 'XMonad.Core.layoutHook' for
reordering the visible windows on the visible workspace(s).

Since different layouts may be attached to different workspaces, and
you can change them, xmonad needs to know which one to use. In this
sense the layoutHook may be thought as the list of layouts that
xmonad will use for laying out windows on the screen(s).

The problem is that the layout subsystem is implemented with an
advanced feature of the Haskell programming language: type classes.
This allows us to very easily write new layouts, combine or modify
existing layouts, create layouts with internal state, etc. See
"XMonad.Doc.Extending#The_LayoutClass" for more information. This
means that we cannot simply have a list of layouts as we used to have
before the 0.5 release: a list requires every member to belong to the
same type!

Instead the combination of layouts to be used by xmonad is created
with a specific layout combinator: 'XMonad.Layouts.|||'.

Suppose we want a list with the 'XMonad.Layouts.Full',
'XMonad.Layout.Tabbed.tabbed' and
'XMonad.Layout.Accordion.Accordion' layouts. First we import, in our
@~\/.xmonad\/xmonad.hs@, all the needed modules:

>    import XMonad
>    import XMonad.Layouts
>
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion

Then we create the combination of layouts we need:

>    mylayoutHook = Full ||| tabbed shrinkText defaultTConf ||| Accordion


Now, all we need to do is change the 'XMonad.Core.layoutHook'
field of the 'XMonad.Core.XConfig' record, like so:

>    main = xmonad $ defaultConfig { layoutHook = mylayoutHook }

Thanks to the new combinator, we can apply a layout modifier to a
whole combination of layouts, instead of applying it to each one. For
example, suppose we want to use the
'XMonad.Layout.NoBorders.noBorders' layout modifier, from the
"XMonad.Layout.NoBorders" module (which must be imported):

>    mylayoutHook = noBorders (Full ||| tabbed shrinkText defaultTConf ||| Accordion)

If we want only the tabbed layout without borders, then we may write:

>    mylayoutHook = Full ||| noBorders (tabbed shrinkText defaultTConf) ||| Accordion

Our @~\/.xmonad\/xmonad.hs@ will now look like this:

>    import XMonad.Layouts
>
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion
>    import XMonad.Layout.NoBorders
>
>    mylayoutHook = Full ||| noBorders (tabbed shrinkText defaultTConf) ||| Accordion
>
>    main = xmonad $ defaultConfig { layoutHook = mylayoutHook }

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
