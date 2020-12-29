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

    -- ** Editing mouse bindings
    -- $mouse

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
(<https://github.com/xmonad/xmonad/issues>)!

-}

{- $actions

In the @XMonad.Actions@ namespace you can find modules exporting
various functions that are usually intended to be bound to key
combinations or mouse actions, in order to provide functionality
beyond the standard keybindings provided by xmonad.

See "XMonad.Doc.Extending#Editing_key_bindings" for instructions on how to
edit your key bindings.

* "XMonad.Actions.AfterDrag":
    Allows you to add actions dependent on the current mouse drag.

* "XMonad.Actions.BluetileCommands":
    External commands for interfacing the [Bluetile](https://hackage.haskell.org/package/bluetile)
    tiling window manager with Xmonad.

* "XMonad.Actions.Commands":
    Allows you to run internal xmonad commands (X () actions) using
    a dmenu menu in addition to key bindings.  Requires dmenu and
    the Dmenu XMonad.Actions module.

* "XMonad.Actions.ConstrainedResize":
    Lets you constrain the aspect ratio of a floating
    window (by, say, holding shift while you resize).
    Useful for making a nice circular XClock window.

* "XMonad.Actions.CopyWindow":
    Provides bindings to duplicate a window on multiple workspaces,
    providing dwm-like tagging functionality.

* "XMonad.Actions.CycleRecentWS":
    Provides bindings to cycle through most recently used workspaces
    with repeated presses of a single key (as long as modifier key is
    held down). This is similar to how many window managers handle
    window switching.

* "XMonad.Actions.CycleSelectedLayouts":
    This module allows to cycle through the given subset of layouts.

* "XMonad.Actions.CycleWS":
    Provides bindings to cycle forward or backward through the list of
    workspaces, to move windows between workspaces, and to cycle
    between screens. Replaces the former XMonad.Actions.RotView.

* "XMonad.Actions.CycleWindows":
    Provides bindings to cycle windows up or down on the current workspace
    stack while maintaining focus in place.

* "XMonad.Actions.DeManage":
    This module provides a method to cease management of a window
    without unmapping it. "XMonad.Hooks.ManageDocks" is a
    more automated solution if your panel supports it.

* "XMonad.Actions.DwmPromote":
    Dwm-like swap function for xmonad.
    Swaps focused window with the master window. If focus is in the
    master, swap it with the next window in the stack. Focus stays in the
    master.

* "XMonad.Actions.DynamicProjects":
    Imbues workspaces with additional features so they can be treated as
    individual project areas.

* "XMonad.Actions.DynamicWorkspaceGroups":
    Dynamically manage "workspace groups", sets of workspaces being used
    together for some common task or purpose, to allow switching between
    workspace groups in a single action. Note that this only makes sense for
    multi-head setups.

* "XMonad.Actions.DynamicWorkspaceOrder":
    Remember a dynamically updateable ordering on workspaces, together with
    tools for using this ordering with "XMonad.Actions.CycleWS" and "XMonad.Hooks.DynamicLog".

* "XMonad.Actions.DynamicWorkspaces":
    Provides bindings to add and delete workspaces.  Note that you may only
    delete a workspace that is already empty.

* "XMonad.Actions.FindEmptyWorkspace":
    Find an empty workspace.

* "XMonad.Actions.FlexibleManipulate":
    Move and resize floating windows without warping the mouse.

* "XMonad.Actions.FlexibleResize":
    Resize floating windows from any corner.

* "XMonad.Actions.FloatKeys":
    Move and resize floating windows.

* "XMonad.Actions.FloatSnap":
    Move and resize floating windows using other windows and the edge of the
    screen as guidelines.

* "XMonad.Actions.FocusNth":
    Focus the nth window of the current workspace.

* "XMonad.Actions.GridSelect":
    GridSelect displays items(e.g. the opened windows) in a 2D grid and lets
    the user select from it with the cursor/hjkl keys or the mouse.

* "XMonad.Actions.GroupNavigation":
    Provides methods for cycling through groups of windows across workspaces,
    ignoring windows that do not belong to this group. A group consists of all
    windows matching a user-provided boolean query. Also provides a method for
    jumping back to the most recently used window in any given group.

* "XMonad.Actions.KeyRemap":
    Remap Keybinding on the fly, e.g having Dvorak char,
    but everything with Control/Shift is left US Layout.

* "XMonad.Actions.Launcher":
    A set of prompts for XMonad.

* "XMonad.Actions.LinkWorkspaces":
    Provides bindings to add and delete links between workspaces. It is aimed at
    providing useful links between workspaces in a multihead setup.
    Linked workspaces are view at the same time.

* "XMonad.Actions.MessageFeedback":
    Alternative to 'XMonad.Operations.sendMessage' that provides knowledge
    of whether the message was handled, and utility functions based on
    this facility.

* "XMonad.Actions.MouseGestures":
    Support for simple mouse gestures.

* "XMonad.Actions.MouseResize":
    A layout modifier to resize windows with the mouse by grabbing the
    window's lower right corner.

* "XMonad.Actions.Navigation2D":
    Navigation2D is an xmonad extension that allows easy directional navigation
    of windows and screens (in a multi-monitor setup).

* "XMonad.Actions.NoBorders":
    This module provides helper functions for dealing with window borders.

* "XMonad.Actions.OnScreen":
    Control workspaces on different screens (in xinerama mode).

* "XMonad.Actions.PerWorkspaceKeys":
    Define key-bindings on per-workspace basis.

* "XMonad.Actions.PhysicalScreens":
    Manipulate screens ordered by physical location instead of ID

* "XMonad.Actions.Plane":
    This module has functions to navigate through workspaces in a bidimensional
    manner.

* "XMonad.Actions.Promote":
    Alternate promote function for xmonad.

* "XMonad.Actions.RandomBackground":
    An action to start terminals with a random background color

* "XMonad.Actions.RotSlaves":
    Rotate all windows except the master window and keep the focus in
    place.

* "XMonad.Actions.Search":
    A module for easily running Internet searches on web sites through xmonad.
    Modeled after the handy Surfraw CLI search tools at <https://secure.wikimedia.org/wikipedia/en/wiki/Surfraw>.

* "XMonad.Actions.ShowText":
    ShowText displays text for sometime on the screen similar to
    "XMonad.Util.Dzen" which offers more features (currently).

* "XMonad.Actions.SimpleDate":
    An example external contrib module for XMonad.
    Provides a simple binding to dzen2 to print the date as a popup menu.

* "XMonad.Actions.SinkAll":
    (Deprecated) Provides a simple binding that pushes all floating windows on the
    current workspace back into tiling. Instead, use the more general
    "XMonad.Actions.WithAll"

* "XMonad.Actions.SpawnOn":
    Provides a way to modify a window spawned by a command(e.g shift it to the workspace
    it was launched on) by using the _NET_WM_PID property that most windows set on creation.

* "XMonad.Actions.Submap":
    A module that allows the user to create a sub-mapping of key bindings.

* "XMonad.Actions.SwapWorkspaces":
    Lets you swap workspace tags, so you can keep related ones next to
    each other, without having to move individual windows.

* "XMonad.Actions.TagWindows":
    Functions for tagging windows and selecting them by tags.

* "XMonad.Actions.TopicSpace":
    Turns your workspaces into a more topic oriented system.

* "XMonad.Actions.TreeSelect":
    TreeSelect displays your workspaces or actions in a Tree-like format.
    You can select the desired workspace/action with the cursor or hjkl keys.
    This module is fully configurable and very useful if you like to have a
    lot of workspaces.

* "XMonad.Actions.UpdateFocus":
    Updates the focus on mouse move in unfocused windows.

* "XMonad.Actions.UpdatePointer":
    Causes the pointer to follow whichever window focus changes to.

* "XMonad.Actions.Warp":
    Warp the pointer to a given window or screen.

* "XMonad.Actions.WindowBringer":
    dmenu operations to bring windows to you, and bring you to windows.
    That is to say, it pops up a dmenu with window names, in case you forgot
    where you left your XChat.

* "XMonad.Actions.WindowGo":
    Defines a few convenient operations for raising (traveling to) windows based on XMonad's Query
    monad, such as 'runOrRaise'.

* "XMonad.Actions.WindowMenu":
    Uses "XMonad.Actions.GridSelect" to display a number of actions related to
    window management in the center of the focused window.

* "XMonad.Actions.WindowNavigation":
    Experimental rewrite of "XMonad.Layout.WindowNavigation".

* "XMonad.Actions.WithAll":
    Provides functions for performing a given action on all windows of
    the current workspace.

* "XMonad.Actions.Workscreen":
    A workscreen permits to display a set of workspaces on several screens. In
    xinerama mode, when a workscreen is viewed, workspaces associated to all
    screens are visible. The first workspace of a workscreen is displayed on
    first screen, second on second screen, etc. Workspace position can be easily
    changed. If the current workscreen is called again, workspaces are shifted.
    This also permits to see all workspaces of a workscreen even if just one screen
    is present, and to move windows from workspace to workscreen.

* "XMonad.Actions.WorkspaceCursors":
    Like "XMonad.Actions.Plane" for an arbitrary number of dimensions.

* "XMonad.Actions.WorkspaceNames":
    Provides bindings to rename workspaces, show these names in DynamicLog and
    swap workspaces along with their names. These names survive restart. Together
    with "XMonad.Layout.WorkspaceDir" this provides for a fully dynamic topic
    space workflow.
-}

{- $configs

In the @XMonad.Config@ namespace you can find modules exporting the
configurations used by some of the xmonad and xmonad-contrib
developers.  You can look at them for examples while creating your own
configuration; you can also simply import them and use them as your
own configuration, possibly with some modifications.


* "XMonad.Config.Arossato":
    This module specifies my xmonad defaults.

* "XMonad.Config.Azerty":
    Fixes some keybindings for users of French keyboard layouts.

* "XMonad.Config.Bepo":
    This module fixes some of the keybindings for the francophone among you who
    use a BEPO keyboard layout. Based on "XMonad.Config.Azerty".

* "XMonad.Config.Bluetile":
    This is the default configuration of [Bluetile](http://projects.haskell.org/bluetile/).
    If you are migrating from Bluetile to xmonad or want to create a similar setup,
    then this will give you pretty much the same thing, except for Bluetile's
    helper applications such as the dock.

* "XMonad.Config.Desktop":
    This module provides core desktop environment settings used
    in the Gnome, Kde, and Xfce config configs. It is also useful
    for people using other environments such as lxde, or using
    tray or panel applications without full desktop environments.

* "XMonad.Config.Dmwit":
    [dmwit](https://github.com/dmwit)'s xmonad configs and helpers.

* "XMonad.Config.Droundy":
    Droundy's xmonad config.

* "XMonad.Config.Gnome":
    This module provides a config suitable for use with the GNOME desktop environment.

* "XMonad.Config.Kde":
    This module provides a config suitable for use with the KDE desktop environment.

* "XMonad.Config.Mate":
    This module provides a config suitable for use with the MATE desktop environment.

* "XMonad.Config.Prime":
    This is a draft of a brand new config syntax for xmonad. It aims to be
    1) easier to copy/paste snippets from the docs 2) easier to get the gist
    for what's going on, for you imperative programmers. It's brand new, so it's
    pretty much guaranteed to break or change syntax. But what's the worst that
    could happen? Xmonad crashes and logs you out? It probably won't do that.
    Give it a try.

* "XMonad.Config.Sjanssen":
    [spencerjanssen](https://github.com/spencerjanssen)'s xmonad configs.

* "XMonad.Config.Xfce":
    This module provides a config suitable for use with the Xfce desktop environment.

* "XMonad.Config.LXQt":
    This module provides a config suitable for use with the LXQt desktop environment.

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

* 'XMonad.Core.handleEventHook': this hook is called on all events handled
  by xmonad, thus it is extremely powerful. See "Graphics.X11.Xlib.Extras"
  and xmonad source and development documentation for more details.

Here is a list of the modules found in @XMonad.Hooks@:

* "XMonad.Hooks.CurrentWorkspaceOnTop":
    Ensures that the windows of the current workspace are always in front of
    windows that are located on other visible screens. This becomes important if
    you use decoration and drag windows from one screen to another.
    Using this module, the dragged window will always be in front of other windows.

* "XMonad.Hooks.DebugEvents":
    Module to dump diagnostic information about X11 events received by xmonad.
    This is incomplete due to "Event" being incomplete and not providing
    information about a number of events, and enforcing artificial constraints
    on others (for example ClientMessage); the X11 package will require a number
    of changes to fix these problems.

* "XMonad.Hooks.DebugKeyEvents":
    A debugging module to track key events, useful when you can't tell whether
    xmonad is processing some or all key events.

* "XMonad.Hooks.DebugStack":
    Dump the state of the StackSet. A logHook and handleEventHook are also provided.

* "XMonad.Hooks.DynamicBars":
    Manage per-screen status bars.

* "XMonad.Hooks.DynamicHooks":
    One-shot and permanent ManageHooks that can be updated at runtime.

* "XMonad.Hooks.DynamicLog": for use with 'XMonad.Core.logHook'; send
  information about xmonad's state to standard output, suitable for
  putting in a status bar of some sort. See
  "XMonad.Doc.Extending#The_log_hook_and_external_status_bars".

* "XMonad.Hooks.EwmhDesktops":
    Makes xmonad use the EWMH hints to tell panel applications about its
    workspaces and the windows therein. It also allows the user to interact
    with xmonad by clicking on panels and window lists.

* "XMonad.Hooks.FadeInactive":
    Makes XMonad set the _NET_WM_WINDOW_OPACITY atom for inactive windows,
    which causes those windows to become slightly translucent if something
    like xcompmgr is running

* "XMonad.Hooks.FadeWindows":
    A more flexible and general compositing interface than FadeInactive. Windows
    can be selected and opacity specified by means of FadeHooks, which are very
    similar to ManageHooks and use the same machinery.

* "XMonad.Hooks.FloatNext":
    Hook and keybindings for automatically sending the next
    spawned window(s) to the floating layer.

* "XMonad.Hooks.ICCCMFocus":
    Deprecated.

* "XMonad.Hooks.InsertPosition":
    Configure where new windows should be added and which window should be
    focused.

* "XMonad.Hooks.ManageDebug":
    A manageHook and associated logHook for debugging "ManageHooks". Simplest
    usage: wrap your xmonad config in the debugManageHook combinator. Or use
    debugManageHookOn for a triggerable version, specifying the triggering key
    sequence in "EZConfig" syntax. Or use the individual hooks in whatever way you see fit.

* "XMonad.Hooks.ManageDocks":
    This module provides tools to automatically manage 'dock' type programs,
    such as gnome-panel, kicker, dzen, and xmobar.

* "XMonad.Hooks.ManageHelpers": provide helper functions to be used
  in @manageHook@.

* "XMonad.Hooks.Minimize":
    Handles window manager hints to minimize and restore windows. Use
    this with XMonad.Layout.Minimize.

* "XMonad.Hooks.Place":
    Automatic placement of floating windows.

* "XMonad.Hooks.PositionStoreHooks":
    This module contains two hooks for the PositionStore (see XMonad.Util.PositionStore) -
    a ManageHook and an EventHook. The ManageHook can be used to fill the
    PositionStore with position and size information about new windows. The advantage
    of using this hook is, that the information is recorded independent of the
    currently active layout. So the floating shape of the window can later be restored
    even if it was opened in a tiled layout initially. The EventHook makes sure
    that windows are deleted from the PositionStore when they are closed.

* "XMonad.Hooks.RestoreMinimized":
    (Deprecated: Use XMonad.Hooks.Minimize) Lets you restore minimized
    windows (see "XMonad.Layout.Minimize") by selecting them on a
    taskbar (listens for _NET_ACTIVE_WINDOW and WM_CHANGE_STATE).

* "XMonad.Hooks.ScreenCorners":
    Run X () actions by touching the edge of your screen with your mouse.

* "XMonad.Hooks.Script":
    Provides a simple interface for running a ~\/.xmonad\/hooks script with the
    name of a hook.

* "XMonad.Hooks.ServerMode":
    Allows sending commands to a running xmonad process.

* "XMonad.Hooks.SetWMName":
    Sets the WM name to a given string, so that it could be detected using
    _NET_SUPPORTING_WM_CHECK protocol.  May be useful for making Java GUI
    programs work.

* "XMonad.Hooks.ToggleHook":
    Hook and keybindings for toggling hook behavior.

* "XMonad.Hooks.UrgencyHook":
    UrgencyHook lets you configure an action to occur when a window demands
    your attention. (In traditional WMs, this takes the form of \"flashing\"
    on your \"taskbar.\" Blech.)

* "XMonad.Hooks.WallpaperSetter":
    Log hook which changes the wallpapers depending on visible workspaces.

* "XMonad.Hooks.WorkspaceByPos":
    Useful in a dual-head setup: Looks at the requested geometry of
    new windows and moves them to the workspace of the non-focused
    screen if necessary.

* "XMonad.Hooks.WorkspaceHistory":
    Keeps track of workspace viewing order.

* "XMonad.Hooks.WindowSwallowing"
    A handleEventHook that implements window swallowing:
    Hide parent windows like terminals when opening other programs (like image viewers) from within them,
    restoring them once the child application closes.

* "XMonad.Hooks.XPropManage":
    A ManageHook matching on XProperties.

-}

{- $layouts

In the @XMonad.Layout@ namespace you can find modules exporting
contributed tiling algorithms, such as a tabbed layout, a circle, a spiral,
three columns, and so on.

You will also find modules which provide facilities for combining
different layouts, such as "XMonad.Layout.Combo", "XMonad.Layout.ComboP",
"XMonad.Layout.LayoutBuilder", "XMonad.Layout.SubLayouts", or
"XMonad.Layout.LayoutCombinators".

Layouts can be also modified with layout modifiers. A general
interface for writing layout modifiers is implemented in
"XMonad.Layout.LayoutModifier".

For more information on using those modules for customizing your
'XMonad.Core.layoutHook' see "XMonad.Doc.Extending#Editing_the_layout_hook".

* "XMonad.Layout.Accordion":
    LayoutClass that puts non-focused windows in ribbons at the top and bottom
    of the screen.

* "XMonad.Layout.AutoMaster":
    Provides layout modifier AutoMaster. It separates screen in two parts -
    master and slave. Size of slave area automatically changes depending on
    number of slave windows.

* "XMonad.Layout.AvoidFloats":
    Find a maximum empty rectangle around floating windows and use that area to
    display non-floating windows.

* "XMonad.Layout.BinarySpacePartition":
    Layout where new windows will split the focused window in half, based off of BSPWM.

* "XMonad.Layout.BorderResize":
    This layout modifier will allow to resize windows by dragging their
    borders with the mouse. However, it only works in layouts or modified
    layouts that react to the SetGeometry message.
    "XMonad.Layout.WindowArranger" can be used to create such a setup.
    BorderResize is probably most useful in floating layouts.

* "XMonad.Layout.BoringWindows":
    BoringWindows is an extension to allow windows to be marked boring

* "XMonad.Layout.ButtonDecoration":
    A decoration that includes small buttons on both ends which invoke various
    actions when clicked on: Show a window menu (see "XMonad.Actions.WindowMenu"),
    minimize, maximize or close the window.

* "XMonad.Layout.CenteredMaster":
    Two layout modifiers. centerMaster places master window at center,
    on top of all other windows, which are managed by base layout.
    topRightMaster is similar, but places master window in top right corner
    instead of center.

* "XMonad.Layout.Circle":
    Circle is an elliptical, overlapping layout.

* "XMonad.Layout.Column":
    Provides Column layout that places all windows in one column. Windows
    heights are calculated from equation: H1/H2 = H2/H3 = ... = q, where q is
    given. With Shrink/Expand messages you can change the q value.

* "XMonad.Layout.Combo":
    A layout that combines multiple layouts.

* "XMonad.Layout.ComboP":
    A layout that combines multiple layouts and allows to specify where to put
    new windows.

* "XMonad.Layout.Cross":
    A Cross Layout with the main window in the center.

* "XMonad.Layout.Decoration":
    A layout modifier and a class for easily creating decorated
    layouts.

* "XMonad.Layout.DecorationAddons":
    Various stuff that can be added to the decoration. Most of it is intended to
    be used by other modules. See "XMonad.Layout.ButtonDecoration" for a module
    that makes use of this.

* "XMonad.Layout.DecorationMadness":
    A collection of decorated layouts: some of them may be nice, some
    usable, others just funny.

* "XMonad.Layout.Dishes":
    Dishes is a layout that stacks extra windows underneath the master
    windows.

* "XMonad.Layout.DragPane":
    Layouts that splits the screen either horizontally or vertically and
    shows two windows.  The first window is always the master window, and
    the other is either the currently focused window or the second window in
    layout order. See also "XMonad.Layout.MouseResizableTall"

* "XMonad.Layout.DraggingVisualizer":
    A helper module to visualize the process of dragging a window by making it
    follow the mouse cursor. See "XMonad.Layout.WindowSwitcherDecoration" for a
    module that makes use of this.

* "XMonad.Layout.Drawer":
    A layout modifier that puts some windows in a "drawer" which retracts and
    expands depending on whether any window in it has focus. Useful for music
    players, tool palettes, etc.

* "XMonad.Layout.Dwindle":
    Three layouts: The first, Spiral, is a reimplementation of spiral with, at
    least to me, more intuitive semantics. The second, Dwindle, is inspired by
    a similar layout in awesome and produces the same sequence of decreasing
    window sizes as Spiral but pushes the smallest windows into a screen corner
    rather than the centre. The third, Squeeze arranges all windows in one row
    or in one column, with geometrically decreasing sizes.

* "XMonad.Layout.DwmStyle":
    A layout modifier for decorating windows in a dwm like style.

* "XMonad.Layout.FixedColumn":
    A layout much like Tall, but using a multiple of a window's minimum
    resize amount instead of a percentage of screen to decide where to
    split. This is useful when you usually leave a text editor or
    terminal in the master pane and like it to be 80 columns wide.

* "XMonad.Layout.Fullscreen":
    Hooks for sending messages about fullscreen windows to layouts, and a few
    example layout modifier that implement fullscreen windows.

* "XMonad.Layout.Gaps":
    Create manually-sized gaps along edges of the screen which will not
    be used for tiling, along with support for toggling gaps on and
    off. You probably want "XMonad.Hooks.ManageDocks".

* "XMonad.Layout.Grid":
    A simple layout that attempts to put all windows in a square grid.

* "XMonad.Layout.GridVariants":
    Two layouts: one is a variant of the Grid layout that allows the
    desired aspect ratio of windows to be specified.  The other is like
    Tall but places a grid with fixed number of rows and columns in the
    master area and uses an aspect-ratio-specified layout for the
    slaves.

* "XMonad.Layout.Groups":
    Two-level layout with windows split in individual layout groups, themselves
    managed by a user-provided layout.

*   * "XMonad.Layout.Groups.Examples":
        Example layouts for "XMonad.Layout.Groups".

*   * "XMonad.Layout.Groups.Helpers":
        Utility functions for "XMonad.Layout.Groups".

*   * "XMonad.Layout.Groups.Wmii":
        A wmii-like layout algorithm.

* "XMonad.Layout.Hidden":
    Similar to XMonad.Layout.Minimize but completely removes windows from the
    window set so XMonad.Layout.BoringWindows isn't necessary. Perfect companion
    to XMonad.Layout.BinarySpacePartition since it can be used to move windows
    to another part of the BSP tree.

* "XMonad.Layout.HintedGrid":
    A not so simple layout that attempts to put all windows in a square grid
    while obeying their size hints.

* "XMonad.Layout.HintedTile":
    A gapless tiled layout that attempts to obey window size hints,
    rather than simply ignoring them.

* "XMonad.Layout.IM":
    Layout modfier suitable for workspace with multi-windowed instant messenger
    (like Psi or Tkabber).

* "XMonad.Layout.IfMax":
    Provides IfMax layout, which will run one layout if there are maximum N
    windows on workspace, and another layout, when number of windows is greater
    than N.

* "XMonad.Layout.ImageButtonDecoration":
    A decoration that includes small image buttons on both ends which invoke
    various actions when clicked on: Show a window menu (see "XMonad.Actions.WindowMenu"),
    minimize, maximize or close the window.

* "XMonad.Layout.IndependentScreens":
    Utility functions for simulating independent sets of workspaces on
    each screen (like dwm's workspace model), using internal tags to
    distinguish workspaces associated with each screen.

* "XMonad.Layout.LayoutBuilder":
    A layout combinator that sends a specified number of windows to one rectangle
    and the rest to another.

* "XMonad.Layout.LayoutCombinators":
    The "XMonad.Layout.LayoutCombinators" module provides combinators
    for easily combining multiple layouts into one composite layout, as
    well as a way to jump directly to any particular layout (say, with
    a keybinding) without having to cycle through other layouts to get
    to it.

* "XMonad.Layout.LayoutHints":
    Make layouts respect size hints.

* "XMonad.Layout.LayoutModifier":
    A module for writing easy layout modifiers, which do not define a
    layout in and of themselves, but modify the behavior of or add new
    functionality to other layouts.  If you ever find yourself writing
    a layout which takes another layout as a parameter, chances are you
    should be writing a LayoutModifier instead!

    In case it is not clear, this module is not intended to help you
    configure xmonad, it is to help you write other extension modules.
    So get hacking!

* "XMonad.Layout.LayoutScreens":
    Divide a single screen into multiple screens.

* "XMonad.Layout.LimitWindows":
    A layout modifier that limits the number of windows that can be shown.

* "XMonad.Layout.MagicFocus":
    Automagically put the focused window in the master area.

* "XMonad.Layout.Magnifier":
    Screenshot  :  <http://caladan.rave.org/magnifier.png>
    This is a layout modifier that will make a layout increase the size
    of the window that has focus.

* "XMonad.Layout.Master":
    Layout modfier that adds a master window to another layout.

* "XMonad.Layout.Maximize":
    Temporarily yanks the focused window out of the layout to mostly fill
    the screen.

* "XMonad.Layout.MessageControl":
    Provides message escaping and filtering facilities which
    help control complex nested layouts.

* "XMonad.Layout.Minimize":
    Makes it possible to minimize windows, temporarily removing them
    from the layout until they are restored.

* "XMonad.Layout.Monitor":
    Layout modfier for displaying some window (monitor) above other windows

* "XMonad.Layout.Mosaic":
    Based on MosaicAlt, but aspect ratio messages always change the aspect
    ratios, and rearranging the window stack changes the window sizes.

* "XMonad.Layout.MosaicAlt":
    A layout which gives each window a specified amount of screen space
    relative to the others. Compared to the 'Mosaic' layout, this one
    divides the space in a more balanced way.

* "XMonad.Layout.MouseResizableTile":
    A layout in the spirit of "XMonad.Layout.ResizableTile", but with the option
    to use the mouse to adjust the layout.

* "XMonad.Layout.MultiColumns":
    This layout tiles windows in a growing number of columns. The number of
    windows in each column can be controlled by messages.

* "XMonad.Layout.MultiToggle":
    Dynamically apply and unapply transformers to your window layout. This can
    be used to rotate your window layout by 90 degrees, or to make the
    currently focused window occupy the whole screen (\"zoom in\") then undo
    the transformation (\"zoom out\").

*   * "XMonad.Layout.MultiToggle.Instances":
        Some convenient common instances of the Transformer class, for use with "XMonad.Layout.MultiToggle".

* "XMonad.Layout.Named":
    A module for assigning a name to a given layout.

* "XMonad.Layout.NoBorders":
    Make a given layout display without borders.  This is useful for
    full-screen or tabbed layouts, where you don't really want to waste a
    couple of pixels of real estate just to inform yourself that the visible
    window has focus.

* "XMonad.Layout.NoFrillsDecoration":
    Most basic version of decoration for windows without any additional
    modifications. In contrast to "XMonad.Layout.SimpleDecoration" this will
    result in title bars that span the entire window instead of being only the
    length of the window title.

* "XMonad.Layout.OnHost":
    Configure layouts on a per-host basis: use layouts and apply layout modifiers
    selectively, depending on the host. Heavily based on "XMonad.Layout.PerWorkspace"
    by Brent Yorgey.

* "XMonad.Layout.OneBig":
    Places one (master) window at top left corner of screen, and other (slave)
    windows at the top.

* "XMonad.Layout.PerScreen":
    Configure layouts based on the width of your screen; use your favorite
    multi-column layout for wide screens and a full-screen layout for small ones.

* "XMonad.Layout.PerWorkspace":
    Configure layouts on a per-workspace basis: use layouts and apply
    layout modifiers selectively, depending on the workspace.

* "XMonad.Layout.PositionStoreFloat":
    A floating layout which has been designed with a dual-head setup in mind.
    It makes use of "XMonad.Util.PositionStore" as well as "XMonad.Hooks.PositionStoreHooks".
    Since there is currently no way to move or resize windows with the keyboard
    alone in this layout, it is adviced to use it in combination with a decoration
    such as "XMonad.Layout.NoFrillsDecoration" (to move windows) and the layout
    modifier "XMonad.Layout.BorderResize" (to resize windows).

* "XMonad.Layout.Reflect":
    Reflect a layout horizontally or vertically.

* "XMonad.Layout.Renamed":
    Layout modifier that can modify the description of its underlying layout on
    a (hopefully) flexible way.

* "XMonad.Layout.ResizableTile":
    More useful tiled layout that allows you to change a width\/height of window.
    See also "XMonad.Layout.MouseResizableTile".

* "XMonad.Layout.ResizableThreeColumns":
    The same layout as ThreeColumns but, similar to ResizableTile, allows you
    to change the width\/height of the slave windows.

* "XMonad.Layout.ResizeScreen":
    A layout transformer to have a layout respect a given screen
    geometry. Mostly used with "Decoration" (the Horizontal and the
    Vertical version will react to SetTheme and change their dimension
    accordingly.

* "XMonad.Layout.Roledex":
    This is a completely pointless layout which acts like Microsoft's Flip 3D

* "XMonad.Layout.ShowWName":
    This is a layout modifier that will show the workspace name

* "XMonad.Layout.SimpleDecoration":
    A layout modifier for adding simple decorations to the windows of a
    given layout. The decorations are in the form of ion-like tabs
    for window titles.

* "XMonad.Layout.SimpleFloat":
    A basic floating layout.

* "XMonad.Layout.Simplest":
    A very simple layout. The simplest, afaik. Used as a base for
    decorated layouts.

* "XMonad.Layout.SimplestFloat":
    A basic floating layout like SimpleFloat but without the decoration.

* "XMonad.Layout.SortedLayout":
    A new LayoutModifier that sorts a given layout by a list of
    properties. The order of properties in the list determines
    the order of windows in the final layout. Any unmatched windows
    go to the end of the order.

* "XMonad.Layout.Spacing":
    Add a configurable amount of space around windows.

* "XMonad.Layout.Spiral":
    A spiral tiling layout.

* "XMonad.Layout.Square":
    A layout that splits the screen into a square area and the rest of the
    screen.
    This is probably only ever useful in combination with
    "XMonad.Layout.Combo".
    It sticks one window in a square region, and makes the rest
    of the windows live with what's left (in a full-screen sense).

* "XMonad.Layout.StackTile":
    A stacking layout, like dishes but with the ability to resize master pane.
    Mostly useful on small screens.

* "XMonad.Layout.Stoppable":
    This module implements a special kind of layout modifier, which when applied
    to a layout, causes xmonad to stop all non-visible processes. In a way,
    this is a sledge-hammer for applications that drain power. For example, given
    a web browser on a stoppable workspace, once the workspace is hidden the web
    browser will be stopped.

* "XMonad.Layout.SubLayouts":
    A layout combinator that allows layouts to be nested.

* "XMonad.Layout.TabBarDecoration":
    A layout modifier to add a bar of tabs to your layouts.

* "XMonad.Layout.Tabbed":
    A tabbed layout for the Xmonad Window Manager

* "XMonad.Layout.ThreeColumns":
    A layout similar to tall but with three columns. With 2560x1600 pixels this
    layout can be used for a huge main window and up to six reasonable sized
    slave windows.

* "XMonad.Layout.ToggleLayouts":
    A module to toggle between two layouts.

* "XMonad.Layout.TwoPane":
    A layout that splits the screen horizontally and shows two windows.  The
    left window is always the master window, and the right is either the
    currently focused window or the second window in layout order.

* "XMonad.Layout.WindowArranger":
    This is a pure layout modifier that will let you move and resize
    windows with the keyboard in any layout.

* "XMonad.Layout.WindowNavigation":
    WindowNavigation is an extension to allow easy navigation of a workspace.
    See also "XMonad.Actions.WindowNavigation".

* "XMonad.Layout.WindowSwitcherDecoration":
    A decoration that allows to switch the position of windows by dragging them
    onto each other.

* "XMonad.Layout.WorkspaceDir":
    WorkspaceDir is an extension to set the current directory in a workspace.
    Actually, it sets the current directory in a layout, since there's no way I
    know of to attach a behavior to a workspace.  This means that any terminals
    (or other programs) pulled up in that workspace (with that layout) will
    execute in that working directory.  Sort of handy, I think.
    Note this extension requires the 'directory' package to be installed.

-}

{- $prompts

In the @XMonad.Prompt@ name space you can find modules providing
graphical prompts for getting user input and using it to perform
various actions.

The "XMonad.Prompt" provides a library for easily writing new prompt
modules.

These are the available prompts:

* "XMonad.Prompt.AppLauncher":
    A module for launch applicationes that receive parameters in the command
    line. The launcher call a prompt to get the parameters.

* "XMonad.Prompt.AppendFile":
    A prompt for appending a single line of text to a file.  Useful for
    keeping a file of notes, things to remember for later, and so on---
    using a keybinding, you can write things down just about as quickly
    as you think of them, so it doesn't have to interrupt whatever else
    you're doing.
    Who knows, it might be useful for other purposes as well!

* "XMonad.Prompt.ConfirmPrompt":
    A module for setting up simple confirmation prompts for keybindings.

* "XMonad.Prompt.DirExec":
    A directory file executables prompt for XMonad. This might be useful if you
    don't want to have scripts in your PATH environment variable (same
    executable names, different behavior) - otherwise you might want to use
    "XMonad.Prompt.Shell" instead - but you want to have easy access to these
    executables through the xmonad's prompt.

* "XMonad.Prompt.Directory":
    A directory prompt for XMonad

* "XMonad.Prompt.Email":
    A prompt for sending quick, one-line emails, via the standard GNU
    \'mail\' utility (which must be in your $PATH).  This module is
    intended mostly as an example of using "XMonad.Prompt.Input" to
    build an action requiring user input.

* "XMonad.Prompt.FuzzyMatch":
    A module for fuzzy completion matching in prompts akin to emacs ido mode

* "XMonad.Prompt.Input":
    A generic framework for prompting the user for input and passing it
    along to some other action.

* "XMonad.Prompt.Layout":
    A layout-selection prompt for XMonad

* "XMonad.Prompt.Man":
    A manual page prompt for XMonad window manager.
    TODO
    * narrow completions by section number, if the one is specified
    (like @\/etc\/bash_completion@ does)

* "XMonad.Prompt.Pass":
    This module provides 3 combinators for ease passwords manipulation (generate, read, remove):
    1) one to lookup passwords in the password-storage.
    2) one to generate a password for a given password label that the user inputs.
    3) one to delete a stored password for a given password label that the user inputs.


* "XMonad.Prompt.RunOrRaise":
    A prompt for XMonad which will run a program, open a file,
    or raise an already running program, depending on context.

* "XMonad.Prompt.Shell":
    A shell prompt for XMonad

* "XMonad.Prompt.Ssh":
    A ssh prompt for XMonad

* "XMonad.Prompt.Theme":
    A prompt for changing the theme of the current workspace

* "XMonad.Prompt.Unicode":
    A prompt for inputting Unicode characters

* "XMonad.Prompt.Window":
    xprompt operations to bring windows to you, and bring you to windows.

* "XMonad.Prompt.Workspace":
    A workspace prompt for XMonad

* "XMonad.Prompt.XMonad":
    A prompt for running XMonad commands

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

* "XMonad.Util.ActionCycle":
    Provides a way to implement cycling actions. This can be used to implement
    things like alternating, toggle-style keybindings.

* "XMonad.Util.ClickableWorkspaces":
    Provides clickablePP, which when applied to the PP pretty-printer used by
    'XMonad.Hooks.DynamicLog.dynamicLogWithPP', will make the workspace tags
    clickable in XMobar (for switching focus).

* "XMonad.Util.Cursor": configure the default cursor/pointer glyph.

* "XMonad.Util.CustomKeys": configure key bindings (see
  "XMonad.Doc.Extending#Editing_key_bindings").

* "XMonad.Util.DebugWindow":
    Module to dump window information for diagnostic/debugging purposes. See
    "XMonad.Hooks.DebugEvents" and "XMonad.Hooks.DebugStack" for practical uses.

* "XMonad.Util.Dmenu":
    A convenient binding to dmenu.
    Requires the process-1.0 package

* "XMonad.Util.Dzen":
    Handy wrapper for dzen. Requires dzen >= 0.2.4.

* "XMonad.Util.EZConfig":
    Configure key bindings easily, including a
    parser for writing key bindings in "M-C-x" style.

* "XMonad.Util.ExtensibleState":
    Module for storing custom mutable state in xmonad.

* "XMonad.Util.Font":
    A module for abstracting a font facility over
    Core fonts and Xft.

* "XMonad.Util.Image":
    Utilities for manipulating [[Bool]] as images.

* "XMonad.Util.Invisible":
    A data type to store the layout state

* "XMonad.Util.Loggers":
    A collection of simple logger functions and formatting utilities
    which can be used in the 'XMonad.Hooks.DynamicLog.ppExtras' field of
    a pretty-printing status logger format. See "XMonad.Hooks.DynamicLog"
    for more information.

*   * "XMonad.Util.Loggers.NamedScratchpad":
        A collection of Loggers (see "XMonad.Util.Loggers") for NamedScratchpads
        (see "XMonad.Util.NamedScratchpad").

* "XMonad.Util.NamedActions":
    A wrapper for keybinding configuration that can list the available
    keybindings.

* "XMonad.Util.NamedScratchpad":
    Like "XMonad.Util.Scratchpad" toggle windows to and from the current
    workspace. Supports several arbitrary applications at the same time.

* "XMonad.Util.NamedWindows":
    This module allows you to associate the X titles of windows with
    them.

* "XMonad.Util.NoTaskbar":
    Utility function and 'ManageHook` to mark a window to be ignored by
    EWMH taskbars and pagers. Useful for `NamedScratchpad` windows, since
    you will usually be taken to the `NSP` workspace by them.

* "XMonad.Util.Paste":
    A module for sending key presses to windows. This modules provides generalized
    and specialized functions for this task.

* "XMonad.Util.PositionStore":
    A utility module to store information about position and size of a window.
    See "XMonad.Layout.PositionStoreFloat" for a layout that makes use of this.

* "XMonad.Util.RemoteWindows":
    This module implements a proper way of finding out whether the window is remote or local.

* "XMonad.Util.Replace":
    Implements a @--replace@ flag outside of core.

* "XMonad.Util.Run":
    This modules provides several commands to run an external process.
    It is composed of functions formerly defined in "XMonad.Util.Dmenu" (by
    Spencer Janssen), "XMonad.Util.Dzen" (by glasser\@mit.edu) and
    XMonad.Util.RunInXTerm (by Andrea Rossato).

* "XMonad.Util.Scratchpad":
    Very handy hotkey-launched toggleable floating terminal window.

* "XMonad.Util.SpawnNamedPipe":
    A module for spawning a pipe whose Handle lives in the Xmonad state.

* "XMonad.Util.SpawnOnce":
    A module for spawning a command once, and only once. Useful to start status
    bars and make session settings inside startupHook.

* "XMonad.Util.Stack":
    Utility functions for manipulating Maybe Stacks.

* "XMonad.Util.StringProp":
    Internal utility functions for storing Strings with the root window.
    Used for global state like IORefs with string keys, but more latency,
    persistent between xmonad restarts.

* "XMonad.Util.Themes":
    A (hopefully) growing collection of themes for decorated layouts.

* "XMonad.Util.Timer":
    A module for setting up timers

* "XMonad.Util.Types":
    Miscellaneous commonly used types.

* "XMonad.Util.Ungrab":
    Release xmonad's keyboard and pointer grabs immediately, so
    screen grabbers and lock utilities, etc. will work. Replaces
    the short sleep hackaround.

* "XMonad.Util.WindowProperties":
    EDSL for specifying window properties; various utilities related to window
    properties.

* "XMonad.Util.WindowState":
    Functions for saving per-window data.

* "XMonad.Util.WorkspaceCompare":
    Functions for examining, comparing, and sorting workspaces.

* "XMonad.Util.XSelection":
    A module for accessing and manipulating X Window's mouse selection (the buffer used in copy and pasting).
    'getSelection' and 'putSelection' are adaptations of Hxsel.hs and Hxput.hs from the XMonad-utils

* "XMonad.Util.XUtils":
    A module for painting on the screen

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

>    import XMonad
>
>    main = xmonad $ def { keys = myKeys }

and provide an appropriate definition of @myKeys@, such as:

> myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
>             [ ((modm, xK_F12), xmonadPrompt def)
>             , ((modm, xK_F3 ), shellPrompt  def)
>             ]

This particular definition also requires importing "XMonad.Prompt",
"XMonad.Prompt.Shell", "XMonad.Prompt.XMonad", and "Data.Map":

> import qualified Data.Map as M
> import XMonad.Prompt
> import XMonad.Prompt.Shell
> import XMonad.Prompt.XMonad

For a list of the names of particular keys (such as xK_F12, and so
on), see
<http://hackage.haskell.org/packages/archive/X11/latest/doc/html/Graphics-X11-Types.html>

Usually, rather than completely redefining the key bindings, as we did
above, we want to simply add some new bindings and\/or remove existing
ones.

-}

{- $keyAdding
#Adding_key_bindings#

Adding key bindings can be done in different ways. See the end of this
section for the easiest ways. The type signature of
'XMonad.Core.XConfig.keys' is:

>    keys :: XConfig Layout -> M.Map (ButtonMask,KeySym) (X ())

In order to add new key bindings, you need to first create an
appropriate 'Data.Map.Map' from a list of key bindings using
'Data.Map.fromList'.  This 'Data.Map.Map' of new key bindings then
needs to be joined to a 'Data.Map.Map' of existing bindings using
'Data.Map.union'.

Since we are going to need some of the functions of the "Data.Map"
module, before starting we must first import this modules:

>    import qualified Data.Map as M


For instance, if you have defined some additional key bindings like
these:

>    myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
>             [ ((modm, xK_F12), xmonadPrompt def)
>             , ((modm, xK_F3 ), shellPrompt  def)
>             ]

then you can create a new key bindings map by joining the default one
with yours:

>    newKeys x  = myKeys x `M.union` keys def x

Finally, you can use @newKeys@ in the 'XMonad.Core.XConfig.keys' field
of the configuration:

>    main = xmonad $ def { keys = newKeys }

Alternatively, the '<+>' operator can be used which in this usage does exactly
the same as the explicit usage of 'M.union' and propagation of the config
argument, thanks to appropriate instances in "Data.Monoid".

>    main = xmonad $ def { keys = myKeys <+> keys def }

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
>    main = xmonad $ def { keys = myKeys <+> keys def }
>
>    myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
>             [ ((modm, xK_F12), xmonadPrompt def)
>             , ((modm, xK_F3 ), shellPrompt  def)
>             ]

There are much simpler ways to accomplish this, however, if you are
willing to use an extension module to help you configure your keys.
For instance, "XMonad.Util.EZConfig" and "XMonad.Util.CustomKeys" both
provide useful functions for editing your key bindings; "XMonad.Util.EZConfig" even lets you use emacs-style keybinding descriptions like \"M-C-<F12>\".

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

>    newKeys x = keys def x `M.difference` keysToRemove x
>
>    keysToRemove :: XConfig Layout ->    M.Map (KeyMask, KeySym) (X ())
>    keysToRemove x = M.fromList
>             [ ((modm              , xK_q ), return ())
>             , ((modm .|. shiftMask, xK_q ), return ())
>             ]

As you can see, it doesn't matter what actions we associate with the
keys listed in @keysToRemove@, so we just use @return ()@ (the
\"null\" action).

It is also possible to simply define a list of keys we want to unbind
and then use 'Data.Map.delete' to remove them. In that case we would
write something like:

>    newKeys x = foldr M.delete (keys def x) (keysToRemove x)
>
>    keysToRemove :: XConfig Layout -> [(KeyMask, KeySym)]
>    keysToRemove x =
>             [ (modm              , xK_q )
>             , (modm .|. shiftMask, xK_q )
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

>    defKeys    = keys def
>    delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
>    newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
>    -- remove some of the default key bindings
>    toRemove XConfig{modMask = modm} =
>        [ (modm              , xK_j     )
>        , (modm              , xK_k     )
>        , (modm              , xK_p     )
>        , (modm .|. shiftMask, xK_p     )
>        , (modm .|. shiftMask, xK_q     )
>        , (modm              , xK_q     )
>        ] ++
>        -- I want modm .|. shiftMask 1-9 to be free!
>        [(shiftMask .|. modm, k) | k <- [xK_1 .. xK_9]]
>    -- These are my personal key bindings
>    toAdd XConfig{modMask = modm} =
>        [ ((modm              , xK_F12   ), xmonadPrompt def )
>        , ((modm              , xK_F3    ), shellPrompt  def )
>        ] ++
>        -- Use modm .|. shiftMask .|. controlMask 1-9 instead
>        [( (m .|. modm, k), windows $ f i)
>         | (i, k) <- zip (workspaces x) [xK_1 .. xK_9]
>        ,  (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask .|. controlMask)]
>        ]

You can achieve the same result using the "XMonad.Util.CustomKeys"
module; take a look at the 'XMonad.Util.CustomKeys.customKeys'
function in particular.

NOTE: modm is defined as the modMask you defined (or left as the default) in
your config.
-}

{- $mouse
#Editing_mouse_bindings#

Most of the previous discussion of key bindings applies to mouse
bindings as well.  For example, you could configure button4 to close
the window you click on like so:

>    import qualified Data.Map as M
>
>    myMouse x  = [ (0, button4), (\w -> focus w >> kill) ]
>
>    newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))
>
>    main = xmonad $ def { ..., mouseBindings = newMouse, ... }

Overriding or deleting mouse bindings works similarly.  You can also
configure mouse bindings much more easily using the
'XMonad.Util.EZConfig.additionalMouseBindings' and
'XMonad.Util.EZConfig.removeMouseBindings' functions from the
"XMonad.Util.EZConfig" module.

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
with a specific layout combinator: 'XMonad.Layout.|||'.

Suppose we want a list with the 'XMonad.Layout.Full',
'XMonad.Layout.Tabbed.tabbed' and
'XMonad.Layout.Accordion.Accordion' layouts. First we import, in our
@~\/.xmonad\/xmonad.hs@, all the needed modules:

>    import XMonad
>
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion

Then we create the combination of layouts we need:

>    mylayoutHook = Full ||| tabbed shrinkText def ||| Accordion


Now, all we need to do is change the 'XMonad.Core.layoutHook'
field of the 'XMonad.Core.XConfig' record, like so:

>    main = xmonad $ def { layoutHook = mylayoutHook }

Thanks to the new combinator, we can apply a layout modifier to a
whole combination of layouts, instead of applying it to each one. For
example, suppose we want to use the
'XMonad.Layout.NoBorders.noBorders' layout modifier, from the
"XMonad.Layout.NoBorders" module (which must be imported):

>    mylayoutHook = noBorders (Full ||| tabbed shrinkText def ||| Accordion)

If we want only the tabbed layout without borders, then we may write:

>    mylayoutHook = Full ||| noBorders (tabbed shrinkText def) ||| Accordion

Our @~\/.xmonad\/xmonad.hs@ will now look like this:

>    import XMonad
>
>    import XMonad.Layout.Tabbed
>    import XMonad.Layout.Accordion
>    import XMonad.Layout.NoBorders
>
>    mylayoutHook = Full ||| noBorders (tabbed shrinkText def) ||| Accordion
>
>    main = xmonad $ def { layoutHook = mylayoutHook }

That's it!

-}

{- $manageHook
#Editing_the_manage_hook#

The 'XMonad.Core.manageHook' is a very powerful tool for customizing
the behavior of xmonad with regard to new windows.  Whenever a new
window is created, xmonad calls the 'XMonad.Core.manageHook', which
can thus be used to perform certain actions on the new window, such as
placing it in a specific workspace, ignoring it, or placing it in the
float layer.

The default 'XMonad.Core.manageHook' causes xmonad to float MPlayer
and Gimp, and to ignore gnome-panel, desktop_window, kicker, and
kdesktop.

The "XMonad.ManageHook" module provides some simple combinators that
can be used to alter the 'XMonad.Core.manageHook' by replacing or adding
to the default actions.

Let's start by analyzing the default 'XMonad.Config.manageHook', defined
in "XMonad.Config":


>    manageHook :: ManageHook
>    manageHook = composeAll
>                    [ className =? "MPlayer"        --> doFloat
>                    , className =? "Gimp"           --> doFloat
>                    , resource  =? "desktop_window" --> doIgnore
>                    , resource  =? "kdesktop"       --> doIgnore ]

'XMonad.ManageHook.composeAll' can be used to compose a list of
different 'XMonad.Config.ManageHook's. In this example we have a list
of 'XMonad.Config.ManageHook's formed by the following commands: the
Mplayer's and the Gimp's windows, whose 'XMonad.ManageHook.className'
are, respectively \"Mplayer\" and \"Gimp\", are to be placed in the
float layer with the 'XMonad.ManageHook.doFloat' function; the windows
whose resource names are respectively \"desktop_window\" and
\kdesktop\" are to be ignored with the 'XMonad.ManageHook.doIgnore'
function.

This is another example of 'XMonad.Config.manageHook', taken from
"XMonad.Config.Arossato":

>    myManageHook  = composeAll [ resource =? "realplay.bin" --> doFloat
>                               , resource =? "win"          --> doF (W.shift "doc") -- xpdf
>                               , resource =? "firefox-bin"  --> doF (W.shift "web")
>                               ]
>    newManageHook = myManageHook <+> manageHook def


Again we use 'XMonad.ManageHook.composeAll' to compose a list of
different 'XMonad.Config.ManageHook's. The first one will put
RealPlayer on the float layer, the second one will put the xpdf
windows in the workspace named \"doc\", with 'XMonad.ManageHook.doF'
and 'XMonad.StackSet.shift' functions, and the third one will put all
firefox windows on the workspace called "web". Then we use the
'XMonad.ManageHook.<+>' combinator to compose @myManageHook@ with the
default 'XMonad.Config.manageHook' to form @newManageHook@.

Each 'XMonad.Config.ManageHook' has the form:

>    property =? match --> action

Where @property@ can be:

* 'XMonad.ManageHook.title': the window's title

* 'XMonad.ManageHook.resource': the resource name

* 'XMonad.ManageHook.className': the resource class name.

* 'XMonad.ManageHook.stringProperty' @somestring@: the contents of the
  property @somestring@.

(You can retrieve the needed information using the X utility named
@xprop@; for example, to find the resource class name, you can type

> xprop | grep WM_CLASS

at a prompt, then click on the window whose resource class you want to
know.)

@match@ is the string that will match the property value (for instance
the one you retrieved with @xprop@).

An  @action@ can be:

* 'XMonad.ManageHook.doFloat': to place the window in the float layer;

* 'XMonad.ManageHook.doIgnore': to ignore the window;

* 'XMonad.ManageHook.doF': to execute a function with the window as
  argument.

For example, suppose we want to add a 'XMonad.Config.manageHook' to
float RealPlayer, which usually has a 'XMonad.ManageHook.resource'
name of \"realplay.bin\".

First we need to import "XMonad.ManageHook":

>    import XMonad.ManageHook

Then we create our own 'XMonad.Config.manageHook':

>    myManageHook = resource =? "realplay.bin" --> doFloat

We can now use the 'XMonad.ManageHook.<+>' combinator to add our
'XMonad.Config.manageHook' to the default one:

>    newManageHook = myManageHook <+> manageHook def

(Of course, if we wanted to completely replace the default
'XMonad.Config.manageHook', this step would not be necessary.) Now,
all we need to do is change the 'XMonad.Core.manageHook' field of the
'XMonad.Core.XConfig' record, like so:

>    main = xmonad def { ..., manageHook = newManageHook, ... }

And we are done.

Obviously, we may wish to add more then one
'XMonad.Config.manageHook'. In this case we can use a list of hooks,
compose them all with 'XMonad.ManageHook.composeAll', and add the
composed to the default one.

For instance, if we want RealPlayer to float and thunderbird always
opened in the workspace named "mail", we can do so like this:

>    myManageHook = composeAll [ resource =? "realplay.bin"    --> doFloat
>                              , resource =? "thunderbird-bin" --> doF (W.shift "mail")
>                              ]

Remember to import the module that defines the 'XMonad.StackSet.shift'
function, "XMonad.StackSet", like this:

>    import qualified XMonad.StackSet as W

And then we can add @myManageHook@ to the default one to create
@newManageHook@ as we did in the previous example.

One more thing to note about this system is that if
a window matches multiple rules in a 'XMonad.Config.manageHook', /all/
of the corresponding actions will be run (in the order in which they
are defined).  This is a change from versions before 0.5, when only
the first rule that matched was run.

Finally, for additional rules and actions you can use in your
manageHook, check out the contrib module "XMonad.Hooks.ManageHelpers".

-}

{- $logHook
#The_log_hook_and_external_status_bars#

When the stack of the windows managed by xmonad changes for any
reason, xmonad will call 'XMonad.Core.logHook', which can be used to
output some information about the internal state of xmonad, such as the
layout that is presently in use, the workspace we are in, the focused
window's title, and so on.

Extracting information about the internal xmonad state can be somewhat
difficult if you are not familiar with the source code. Therefore,
it's usually easiest to use a module that has been designed
specifically for logging some of the most interesting information
about the internal state of xmonad: "XMonad.Hooks.DynamicLog".  This
module can be used with an external status bar to print the produced
logs in a convenient way; the most commonly used status bars are dzen
and xmobar.

By default the 'XMonad.Core.logHook' doesn't produce anything. To
enable it you need first to import "XMonad.Hooks.DynamicLog":

>    import XMonad.Hooks.DynamicLog

Then you just need to update the 'XMonad.Core.logHook' field of the
'XMonad.Core.XConfig' record with one of the provided functions. For
example:

>    main = xmonad def { logHook = dynamicLog }

More interesting configurations are also possible; see the
"XMonad.Hooks.DynamicLog" module for more possibilities.

You may now enjoy your extended xmonad experience.

Have fun!

-}
