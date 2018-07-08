# Change Log / Release Notes

## 0.14 (Not Yet)

### Breaking Changes

  * `XMonad.Layout.Spacing`

    Rewrite `XMonad.Layout.Spacing`. Borders are no longer uniform but composed
    of four sides each with its own border width. The screen and window borders
    are now separate and can be independently toggled on/off. The screen border
    examines the window/rectangle list resulting from 'runLayout' rather than
    the stack, which makes it compatible with layouts such as the builtin
    `Full`. The child layout will always be called with the screen border. If
    only a single window is displayed (and `smartBorder` enabled), it will be
    expanded into the original layout rectangle. Windows that are displayed but
    not part of the stack, such as those created by 'XMonad.Layout.Decoration',
    will be shifted out of the way, but not scaled (not possible for windows
    created by XMonad). This isn't perfect, so you might want to disable
    `Spacing` on such layouts.

  * `XMonad.Util.SpawnOnce`

    - Added `spawnOnOnce`, `spawnNOnOnce` and `spawnAndDoOnce`. These are useful in startup hooks
      to shift spawned windows to a specific workspace.

  * Adding handling of modifySpacing message in smartSpacing and smartSpacingWithEdge layout modifier

  * `XMonad.Actions.GridSelect`

    - Added field `gs_bordercolor` to `GSConfig` to specify border color.

  * `ewmh` function from `X.H.EwmhDesktops` will use `manageHook` for handling
    activated window. That means, actions, which you don't want to happen on
    activated windows, should be guarded by

        not <$> activated

    predicate. By default, with empty `ManageHook`, window activation will do
    nothing.

    Also, you can use regular 'ManageHook' combinators for changing window
    activation behavior.

  * `XMonad.Layout.Minimize`

     Though the interface it offers is quite similar, this module has been
     almost completely rewritten. The new `XMonad.Actions.Minimize` contains
     several functions that allow interaction with minimization window state.
     If you are using this module, you must upgrade your configuration to import
     `X.A.Minimize` and use `maximizeWindow` and `withLastMinimized` instead of
     sending messages to `Minimized` layout. `XMonad.Hooks.RestoreMinimized` has
     been completely deprecated, and its functions have no effect.

  * `XMonad.Prompt.Unicode`

    - `unicodePrompt :: String -> XPConfig -> X ()` now additionally takes a
      filepath to the `UnicodeData.txt` file containing unicode data.

  * `XMonad.Actions.PhysicalScreen`

    `getScreen`, `viewScreen`, `sendToScreen`, `onNextNeighbour`, `onPrevNeighbour` now need a extra parameter
    of type `ScreenComparator`. This allow the user to specify how he want his screen to be ordered default
    value are:

     - `def`(same as verticalScreenOrderer) will keep previous behavior
     - `verticalScreenOrderer`
     - `horizontalScreenOrderer`

    One can build his custom ScreenOrderer using:
     - `screenComparatorById` (allow to order by Xinerama id)
     - `screenComparatorByRectangle` (allow to order by screen coordonate)
     - `ScreenComparator` (allow to mix ordering by screen coordonate and xinerama id)

  * `XMonad.Util.WorkspaceCompare`

    `getXineramaPhysicalWsCompare` now need a extra argument of type `ScreenComparator` defined in
    `XMonad.Actions.PhysicalScreen` (see changelog of this module for more information)

  * `XMonad.Hooks.EwmhDesktops`

    - Simplify ewmhDesktopsLogHookCustom, and remove the gnome-panel specific
      remapping of all visible windows to the active workspace (#216).
    - Handle workspace renames that might be occuring in the custom function
      that is provided to ewmhDesktopsLogHookCustom.

  * `XMonad.Hooks.DynamicLog`

    - Support xmobar's \<action> and \<raw> tags; see `xmobarAction` and
      `xmobarRaw`.

  * `XMonad.Layout.NoBorders`

    The layout now maintains a list of windows that never have borders, and a
    list of windows that always have borders. Use `BorderMessage` to manage
    these lists and the accompanying event hook (`borderEventHook`) to remove
    destroyed windows from them. Also provides the `hasBorder` manage hook.

    Two new conditions have been added to `Ambiguity`: `OnlyLayoutFloat` and
    `OnlyLayoutFloatBelow`; `OnlyFloat` was renamed to `OnlyScreenFloat`.  See
    the documentation for more information.

    The type signature of `hiddens` was changed to accept a new `Rectangle`
    parameter representing the bounds of the parent layout, placed after the
    `WindowSet` parameter. Anyone defining a new instance of `SetsAmbiguous`
    will need to update their configuration. For example, replace "`hiddens amb
    wset mst wrs =`" either with "`hiddens amb wset _ mst wrs =`" or to make
    use of the new parameter with "`hiddens amb wset lr mst wrs =`".

### New Modules

  * `XMonad.Hooks.RefocusLast`

    Provides log and event hooks that keep track of recently focused windows on
    a per workspace basis and automatically refocus the last window when the
    current one is closed. Also provides an action to toggle focus between the
    current and previous window, and one that refocuses appropriately on sending
    the current window to another workspace.

  * `XMonad.Layout.StateFull`

    Provides StateFull: a stateful form of Full that does not misbehave when
    floats are focused, and the FocusTracking layout transformer by means of
    which StateFull is implemented. FocusTracking simply holds onto the last
    true focus it was given and continues to use it as the focus for the
    transformed layout until it sees another. It can be used to improve the
    behaviour of a child layout that has not been given the focused window.

  * `XMonad.Actions.SwapPromote`

    Module for tracking master window history per workspace, and associated
    functions for manipulating the stack using such history.

  * `XMonad.Hooks.Focus`

    A new module extending ManageHook EDSL to work on focused windows and
    current workspace.

    This module will enable window activation (`_NET_ACTIVE_WINDOW`) and apply
    `manageHook` to activated window too. Thus, it may lead to unexpected
    results, when `manageHook` previously working only for new windows, start
    working for activated windows too. It may be solved, by adding
    `not <$> activated` before those part of `manageHook`, which should not be
    called for activated windows.  But this lifts `manageHook` into
    `FocusHook` and it needs to be converted back later using `manageFocus`.

  * `XMonad.Actions.CycleWorkspaceByScreen`

    A new module that allows cycling through previously viewed workspaces in the
    order they were viewed most recently on the screen where cycling is taking
    place.

    Also provides the `repeatableAction` helper function which can be used to
    build actions that can be repeated while a modifier key is held down.

  * `XMonad.Prompt.FuzzyMatch`

    Provides a predicate 'fuzzyMatch' that is much more lenient in matching
    completions in XMonad.Prompt than the default prefix match.  Also provides
    a function 'fuzzySort' that allows sorting the fuzzy matches by "how well"
    they match.

  * `XMonad.Utils.SessionStart`

    A new module that allows to query if this is the first time xmonad is
    started of the session, or a xmonad restart.

    Currently needs manual setting of the session start flag. This could be
    automated when this moves to the core repository.

  * `XMonad.Layout.MultiDishes`

    A new layout based on Dishes, however it accepts additional configuration
    to allow multiple windows within a single stack.

  * `XMonad.Util.Rectangle`

    A new module for handling pixel rectangles.

  * `XMonad.Layout.BinaryColumn`

    A new module which provides a simple grid layout, halving the window
    sizes of each window after master.

    This is similar to Column, but splits the window in a way
    that maintains window sizes upon adding & removing windows as well as the
    option to specify a minimum window size.

### Bug Fixes and Minor Changes

  * `XMonad.Layout.Grid`

    Fix as per issue #223; Grid will no longer calculate more columns than there
    are windows.

  * XMonad.Hooks.FadeWindows

    Added support for GHC version 8.4.x by adding a Semigroup instance for
    Monoids

  * XMonad.Hooks.WallpaperSetter

    Added support for GHC version 8.4.x by adding a Semigroup instance for
    Monoids

  * XMonad.Hooks.Mosaic

    Added support for GHC version 8.4.x by adding a Semigroup instance for
    Monoids

  * `XMonad.Actions.Navigation2D`

    Added `sideNavigation` and a parameterised variant, providing a navigation
    strategy with fewer quirks for tiled layouts using X.L.Spacing.

  * `XMonad.Layout.Fullscreen`
    
    The fullscreen layouts will now not render any window that is totally
    obscured by fullscreen windows.
    
  * `XMonad.Layout.Gaps`

    Extended the sendMessage interface with `ModifyGaps` to allow arbitrary
    modifications to the `GapSpec`.

  * `XMonad.Layout.Groups`

    Added a new `ModifyX` message type that allows the modifying
    function to return values in the `X` monad.

  * `XMonad.Actions.Navigation2D`

    Generalised (and hence deprecated) hybridNavigation to hybridOf.

  * `XMonad.Layout.LayoutHints`

    Preserve the window order of the modified layout, except for the focused
    window that is placed on top. This fixes an issue where the border of the
    focused window in certain situations could be rendered below borders of
    unfocused windows. It also has a lower risk of interfering with the
    modified layout.

  * `XMonad.Layout.MultiColumns`

    The focused window is placed above the other windows if they would be made to
    overlap due to a layout modifier. (As long as it preserves the window order.)

  * `XMonad.Actions.GridSelect`

    - The vertical centring of text in each cell has been improved.

  * `XMonad.Actions.SpawnOn`

    - Bind windows spawns by child processes of the original window to the same
      workspace as the original window.

  * `XMonad.Util.WindowProperties`

    - Added the ability to test if a window has a tag from
      `XMonad.Actions.TagWindows`

  * `XMonad.Layout.Magnifier`

    - Handle `IncMasterN` messages.

  * `XMonad.Util.EZConfig`

    - Can now parse Latin1 keys, to better accommodate users with
      non-US keyboards.

  * `XMonad.Actions.Submap`

    Establish pointer grab to avoid freezing X, when button press occurs after
    submap key press.  And terminate submap at button press in the same way,
    as we do for wrong key press.

  * `XMonad.Hooks.SetWMName`

    Add function `getWMName`.

  * `XMonad.Hooks.ManageHelpers`

    Make type of ManageHook combinators more general.

  * `XMonad.Prompt`

    Export `insertString`.

  * `XMonad.Prompt.Window`

    - New function: `windowMultiPrompt` for using `mkXPromptWithModes`
      with window prompts.

  * `XMonad.Hooks.WorkspaceHistory`

    - Now supports per screen history.

  * `XMonad.Layout.ComboP`

    - New `PartitionWins` message to re-partition all windows into the
      configured sub-layouts.  Useful when window properties have
      changed and you want to re-sort windows into the appropriate
      sub-layout.

  * `XMonad.Actions.Minimize`

    - Now has `withFirstMinimized` and `withFirstMinimized'` so you can perform
      actions with both the last and first minimized windows easily.

  * `XMonad.Config.Gnome`

    - Update logout key combination (modm+shift+Q) to work with modern

  * `XMonad.Prompt.Pass`

    - New function `passTypePrompt` which uses `xdotool` to type in a password
      from the store, bypassing the clipboard.
    - Now handles password labels with spaces and special characters inside
      them.

  * `XMonad.Prompt.Unicode`

    - Persist unicode data cache across XMonad instances due to
      `ExtensibleState` now used instead of `unsafePerformIO`.
    - `typeUnicodePrompt :: String -> XPConfig -> X ()` provided to insert the
      Unicode character via `xdotool` instead of copying it to the paste buffer.
    - `mkUnicodePrompt :: String -> [String] -> String -> XPConfig -> X ()`
      acts as a generic function to pass the selected Unicode character to any
      program.

  * `XMonad.Prompt.AppendFile`

    - New function `appendFilePrompt'` which allows for transformation of the
      string passed by a user before writing to a file.

  * `XMonad.Hooks.DynamicLog`

    - Added a new function `dzenWithFlags` which allows specifying the arguments
    passed to `dzen2` invocation. The behaviour of current `dzen` function is
    unchanged.

  * `XMonad.Util.Dzen`

    - Now provides functions `fgColor` and `bgColor` to specify foreground and
    background color, `align` and `slaveAlign` to set text alignment, and
    `lineCount` to enable a second (slave) window that displays lines beyond
    the initial (title) one.

  * `XMonad.Hooks.DynamicLog`

    - Added optional `ppVisibleNoWindows` to differentiate between empty
      and non-empty visible workspaces in pretty printing.

  * `XMonad.Actions.DynamicWorkspaceOrder`

    - Added `updateName` and `removeName` to better control ordering when
      workspace names are changed or workspaces are removed.

## 0.13 (February 10, 2017)

### Breaking Changes

  * The type of `completionKey` (of `XPConfig` record) has been
    changed from `KeySym` to `(KeyMask, KeySym)`. The default value
    for this is still bound to the `Tab` key.

  * New constructor `CenteredAt Rational Rational` added for
    `XMonad.Prompt.XPPosition`.

  * `XMonad.Prompt` now stores its history file in the XMonad cache
    directory in a file named `prompt-history`.

### New Modules

  * `XMonad.Layout.SortedLayout`

    A new LayoutModifier that sorts a given layout by a list of
    properties. The order of properties in the list determines
    the order of windows in the final layout. Any unmatched windows
    go to the end of the order.

  * `XMonad.Prompt.Unicode`

    A prompt to search a unicode character by its name, and put it into the
    clipboard.

  * `XMonad.Util.Ungrab`

    Release xmonad's keyboard and pointer grabs immediately, so
    screen grabbers and lock utilities, etc. will work. Replaces
    the short sleep hackaround.

  * `XMonad.Util.Loggers.NamedScratchpad`

    A collection of Loggers (see `XMonad.Util.Loggers`) for NamedScratchpads
    (see `XMonad.Util.NamedScratchpad`).

  * `XMonad.Util.NoTaskbar`

    Utility function and `ManageHook` to mark a window to be ignored by
    EWMH taskbars and pagers. Useful for `NamedScratchpad` windows, since
    you will usually be taken to the `NSP` workspace by them.

### Bug Fixes and Minor Changes

  * `XMonad.Hooks.ManageDocks`,

    - Fix a very annoying bug where taskbars/docs would be
      covered by windows.

    - Also fix a bug that caused certain Gtk and Qt application to
      have issues displaying menus and popups.

  * `XMonad.Layout.LayoutBuilder`

    Merge all functionality from `XMonad.Layout.LayoutBuilderP` into
    `XMonad.Layout.LayoutBuilder`.

  * `XMonad.Actions.WindowGo`

    - Fix `raiseNextMaybe` cycling between 2 workspaces only.

  * `XMonad.Actions.UpdatePointer`

    - Fix bug when cursor gets stuck in one of the corners.

  * `XMonad.Actions.DynamicProjects`

    - Switching away from a dynamic project that contains no windows
      automatically deletes that project's workspace.

      The project itself was already being deleted, this just deletes
      the workspace created for it as well.

    - Added function to change the working directory (`changeProjectDirPrompt`)

    - All of the prompts are now multiple mode prompts.  Try using the
      `changeModeKey` in a prompt and see what happens!

## 0.12 (December 14, 2015)

### Breaking Changes

  * `XMonad.Actions.UpdatePointer.updatePointer` arguments were
    changed. This allows including aspects of both of the
    `TowardsCentre` and `Relative` methods. To keep the same behavior,
    replace the entry in the left column with the entry on the right:

    | < 0.12                              |   >= 0.12                        |
    |-------------------------------------|----------------------------------|
    | `updatePointer Nearest`             | `updatePointer (0.5, 0.5) (1,1)` |
    | `updatePointer (Relative x y)`      | `updatePointer (x,y) (0,0)`      |
    | `updatePointer (TowardsCentre x y)` | `updatePointer (0.5,0.5) (x,y)`  |

### New Modules

  * `XMonad.Actions.AfterDrag`

    Perform an action after the current mouse drag is completed.

  * `XMonad.Actions.DynamicProjects`

    Imbues workspaces with additional features so they can be treated
    as individual project areas.

  * `XMonad.Actions.LinkWorkspaces`

    Provides bindings to add and delete links between workspaces. It
    is aimed at providing useful links between workspaces in a
    multihead setup. Linked workspaces are viewed at the same time.

  * `XMonad.Config.Bepo`

    This module fixes some of the keybindings for the francophone
    among you who use a BEPO keyboard layout. Based on
    `XMonad.Config.Azerty`

  * `XMonad.Config.Dmwit`

    Daniel Wagner's configuration.

  * `XMonad.Config.Mate`

    This module provides a config suitable for use with the MATE
    desktop environment.

  * `XMonad.Config.Prime`

    A draft of a brand new config syntax for xmonad.

  * `XMonad.Hooks.DynamicProperty`

    Module to apply a `ManageHook` to an already-mapped window when a
    property changes. This would commonly be used to match browser
    windows by title, since the final title will only be set after (a)
    the window is mapped, (b) its document has been loaded, (c) all
    load-time scripts have run.

  * `XMonad.Hooks.ManageDebug`

    A `manageHook` and associated `logHook` for debugging `ManageHook`s.
    Simplest usage: wrap your xmonad config in the `debugManageHook`
    combinator.  Or use `debugManageHookOn` for a triggerable version,
    specifying the triggering key sequence in `XMonad.Util.EZConfig`
    syntax. Or use the individual hooks in whatever way you see fit.

  * `XMonad.Hooks.WallpaperSetter`

    Log hook which changes the wallpapers depending on visible
    workspaces.

  * `XMonad.Hooks.WorkspaceHistory`

    Keeps track of workspace viewing order.

  * `XMonad.Layout.AvoidFloats`

    Find a maximum empty rectangle around floating windows and use
    that area to display non-floating windows.

  * `XMonad.Layout.BinarySpacePartition`

    Layout where new windows will split the focused window in half,
    based off of BSPWM.

  * `XMonad.Layout.Dwindle`

    Three layouts: The first, `Spiral`, is a reimplementation of
    `XMonad.Layout.Spiral.spiral` with, at least to me, more intuitive
    semantics.  The second, `Dwindle`, is inspired by a similar layout
    in awesome and produces the same sequence of decreasing window
    sizes as Spiral but pushes the smallest windows into a screen
    corner rather than the centre.  The third, `Squeeze` arranges all
    windows in one row or in one column, with geometrically decreasing
    sizes.

  * `XMonad.Layout.Hidden`

    Similar to `XMonad.Layout.Minimize` but completely removes windows
    from the window set so `XMonad.Layout.BoringWindows` isn't
    necessary.  Perfect companion to `XMonad.Layout.BinarySpacePartition`
    since it can be used to move windows to another part of the BSP tree.

  * `XMonad.Layout.IfMax`

    Provides `IfMax` layout, which will run one layout if there are
    maximum `N` windows on workspace, and another layout, when number
    of windows is greater than `N`.

  * `XMonad.Layout.PerScreen`

    Configure layouts based on the width of your screen; use your
    favorite multi-column layout for wide screens and a full-screen
    layout for small ones.

  * `XMonad.Layout.Stoppable`

    This module implements a special kind of layout modifier, which when
    applied to a layout, causes xmonad to stop all non-visible processes.
    In a way, this is a sledge-hammer for applications that drain power.
    For example, given a web browser on a stoppable workspace, once the
    workspace is hidden the web browser will be stopped.

  * `XMonad.Prompt.ConfirmPrompt`

    A module for setting up simple confirmation prompts for
    keybindings.

  * `XMonad.Prompt.Pass`

    This module provides 3 `XMonad.Prompt`s to ease passwords
    manipulation (generate, read, remove) via [pass][].

  * `XMonad.Util.RemoteWindows`

    This module implements a proper way of finding out whether the
    window is remote or local.

  * `XMonad.Util.SpawnNamedPipe`

    A module for spawning a pipe whose `Handle` lives in the xmonad state.

  * `XMonad.Util.WindowState`

    Functions for saving per-window data.

### Miscellaneous Changes

  * Fix issue #9: `XMonad.Prompt.Shell` `searchPredicate` is ignored,
    defaults to `isPrefixOf`

  * Fix moveHistory when alwaysHighlight is enabled

  * `XMonad.Actions.DynamicWorkspaceGroups` now exports `addRawWSGroup`

  * Side tabs were added to the tabbed layout

  * `XMonad/Layout/IndependentScreens` now exports `marshallSort`

  * `XMonad/Hooks/UrgencyHook` now exports `clearUrgency`

  * Exceptions are now caught when finding commands on `PATH` in `Prompt.Shell`

  * Switched to `Data.Default` wherever possible

  * `XMonad.Layout.IndependentScreens` now exports `whenCurrentOn`

  * `XMonad.Util.NamedActions` now exports `addDescrKeys'`

  * EWMH `DEMANDS_ATTENTION` support added to `UrgencyHook`

  * New `useTransientFor` modifier in `XMonad.Layout.TrackFloating`

  * Added the ability to remove arbitrary workspaces

## 0.9 (October 26, 2009)

### Updates that Require Changes in `xmonad.hs`

  * `XMonad.Hooks.EwmhDesktops` no longer uses `layoutHook`, the
    `ewmhDesktopsLayout` modifier has been removed from
    xmonad-contrib. It uses `logHook`, `handleEventHook`, and
    `startupHook` instead and provides a convenient function `ewmh` to
    add EWMH support to a `defaultConfig`.

  * Most `DynamicLog` users can continue with configs unchanged, but
    users of the quickbar functions `xmobar` or `dzen` will need to
    change `xmonad.hs`: their types have changed to allow easier
    composition with other `XConfig` modifiers. The `dynamicLogDzen`
    and `dynamicLogXmobar` functions have been removed.

  * `WindowGo` or `safeSpawn` users may need to change command lines
    due to `safeSpawn` changes.

  * People explicitly referencing the "SP" scratchpad workspace should
    change it to "NSP" which is also used by the new
    `Util.NamedScratchpad` module.

  * (Optional) People who explicitly use `swapMaster` in key or mouse
    bindings should change it to `shiftMaster`. It's the current
    default used where `swapMaster` had been used previously. It works
    better than `swapMaster` when using floating and tiled windows
    together on the same workspace.

## See Also

<https://wiki.haskell.org/Xmonad/Notable_changes_since_0.8>

[pass]: http://www.passwordstore.org/
