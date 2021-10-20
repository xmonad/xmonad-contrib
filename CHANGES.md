# Change Log / Release Notes

## unknown

### Breaking Changes

  * All modules that export bitmap fonts as their default

    - If xmonad is compiled with XFT support (the default), use an XFT
      font instead.  The previous default expected an X11 misc font
      (PCF), which is not supported in pango 1.44 anymore and thus some
      distributions have stopped shipping these.

      This fixes the silent `user error (createFontSet)`; this would
      break the respective modules.

  * `XMonad.Prompt`

    - Now `mkComplFunFromList` and `mkComplFunFromList'` take an
      additional `XPConfig` argument, so that they can take into
      account the given `searchPredicate`.

    - A `complCaseSensitivity` field has been added to `XPConfig`, indicating
      whether case-sensitivity is desired when performing completion.

    - `historyCompletion` and `historyCompletionP` now both have an `X`
      constraint (was: `IO`), due to changes in how the xmonad core handles XDG
      directories.

    - The prompt window now sets a `WM_CLASS` property.  This allows
      other applications, like compositors, to properly match on it.

  * `XMonad.Hooks.EwmhDesktops`

    - It is no longer recommended to use `fullscreenEventHook` directly.
      Instead, use `ewmhFullscreen` which additionally advertises fullscreen
      support in `_NET_SUPPORTED` and fixes fullscreening of applications that
      explicitly check it, e.g. mupdf-gl, sxiv, …

      `XMonad.Layout.Fullscreen.fullscreenSupport` now advertises it as well,
      and no configuration changes are required in this case.

    - Deprecated `ewmhDesktopsLogHookCustom` and `ewmhDesktopsEventHookCustom`;
      these are now replaced by a composable `XMonad.Util.ExtensibleConf`-based
      interface. Users are advised to just use the `ewmh` XConfig combinator
      and customize behaviour using the provided `addEwmhWorkspaceSort`,
      `addEwmhWorkspaceRename` functions, or better still, use integrations
      provided by modules such as `XMonad.Actions.WorkspaceNames`.

      This interface now additionally allows customization of what happens
      when clients request window activation. This can be used to ignore
      activation of annoying applications, to mark windows as urgent instead
      of focusing them, and more. There's also a new `XMonad.Hooks.Focus`
      module extending the ManageHook EDSL with useful combinators.

    - Ordering of windows that are set to `_NET_CLIENT_LIST` and `_NET_CLIENT_LIST_STACKING`
      was changed to be closer to the spec. From now these two lists will have
      differently sorted windows.

    - `_NET_WM_STATE_DEMANDS_ATTENTION` was added to the list of supported
      hints (as per `_NET_SUPPORTED`). This hint has long been understood by
      `UrgencyHook`. This enables certain applications (e.g. kitty terminal
      emulator) that check whether the hint is supported to use it.

  * All modules still exporting a `defaultFoo` constructor

    - All of these were now removed. You can use the re-exported `def` from
      `Data.Default` instead.

  * `XMonad.Hooks.Script`

    - `execScriptHook` now has an `X` constraint (was: `MonadIO`), due to changes
      in how the xmonad core handles XDG directories.

  * `XMonad.Actions.WorkspaceNames`

    - The type of `getWorkspaceNames` was changed to fit into the new `ppRename`
      field of `PP`.

  * `XMonad.Hooks.StatusBar`, `XMonad.Hooks.StatusBar.PP` (previously
    `XMonad.Hooks.DynamicLog`) and `XMonad.Util.Run`

    - `spawnPipe` no longer uses binary mode handles but defaults to the
      current locale encoding instead.

      `dynamicLogString`, the output of which usually goes directly into such
      a handle, no longer encodes its output in UTF-8, but returns a normal
      `String` of Unicode codepoints instead.

      When these two are used together, everything should continue to work as
      it always has, but in isolation behaviour might change.

      (To get the old `spawnPipe` behaviour, `spawnPipeWithNoEncoding` can now
      be used, and `spawnPipeWithUtf8Encoding` was added as well to force
      UTF-8 regardless of locale. These shouldn't normally be necessary, though.)

    - `xmonadPropLog` and `xmonadPropLog'` now encode the String in UTF-8.
      Again, no change when used together with `dynamicLogString`, but other
      uses of these in user configs might need to be adapted.

  * `XMonad.Actions.TopicSpace`

    - Deprecated the `maxTopicHistory` field, as well as the
      `getLastFocusedTopics` and `setLastFocusedTopic` functions.  It is
      now recommended to directly use `XMonad.Hooks.WorkspaceHistory`
      instead.

    - Added `TopicItem`, as well as the helper functions `topicNames`,
      `tiActions`, `tiDirs`, `noAction`, and `inHome` for a more
      convenient specification of topics.

  * `XMonad.Actions.CycleRecentWS`

    - Changed the signature of `recentWS` to return a `[WorkspaceId]`
      instead of a `[WindowSet]`, while `cycleWindowSets` and
      `toggleWindowSets` now take a function `WindowSet ->
      [WorkspaceId]` instead of one to `[WindowSet]` as their first
      argument.  This fixes the interplay between this module and any
      layout that stores state.

  * `XMonad.Layout.LayoutCombinators`

    - Moved the alternative `(|||)` function and `JumpToLayout` to the
      xmonad core.  They are re-exported by the module, but do not add any
      new functionality.  `NewSelect` now exists as a deprecated type
      alias to `Choose`.

    - Removed the `Wrap` and `NextLayoutNoWrap` data constructors.

  - `XMonad.Actions.CycleWS`

    - Deprecated `EmptyWS`, `HiddenWS`, `NonEmptyWS`, `HiddenNonEmptyWS`,
      `HiddenEmptyWS`, `AnyWS` and `WSTagGroup`.

  - `XMonad.Actions.GridSelect`

    - `colorRangeFromClassName` now uses different hash function,
      so colors of inactive window tiles will be different (but still inside
      the provided color range).

  * `XMonad.Actions.Search`

    - Removed outdated `isohunt` search engine.

    - Updated URLs for `codesearch`, `openstreetmap`, and `thesaurus`
      search engines.

    - Added `github` search engine.

### New Modules

  * `XMonad.Layout.FixedAspectRatio`

    Layout modifier for user provided per-window aspect ratios.

  * `XMonad.Hooks.TaffybarPagerHints`

    Add a module that exports information about XMonads internal state that is
    not available through EWMH that is used by the taffybar status bar.

  * `XMonad.Hooks.StatusBar.PP`

    Originally contained inside `XMonad.Hooks.DynamicLog`, this module provides the
    pretty-printing abstraction and utilities, used primarly with `logHook`.

    Below are changes from `XMonad.Hooks.DynamicLog`, that now are included in
    this module:

    - Added `shortenLeft` function, like existing `shorten` but shortens by
      truncating from left instead of right. Useful for showing directories.

    - Added `shorten'` and `shortenLeft'` functions with customizable overflow
      markers.

    - Added `filterOutWsPP` for filtering out certain workspaces from being
      displayed.

    - Added `xmobarBorder` for creating borders around strings and
      `xmobarFont` for selecting an alternative font.

    - Added `ppRename` to `PP`, which makes it possible for extensions like
      `workspaceNamesPP`, `marshallPP` and/or `clickablePP` to compose
      intuitively.

  * `XMonad.Hooks.StatusBar`

    This module provides a new interface that replaces `XMonad.Hooks.DynamicLog`,
    by providing composoble status bars. Supports property-based as well
    as pipe-based status bars.

  * `XMonad.Util.Hacks`

    A collection of hacks and fixes that should be easily acessible to users:

    - `windowedFullscreenFix` fixes fullscreen behaviour of chromium based
      applications when using windowed fullscreen.

    - `javaHack` helps when dealing with Java applications that might not work
      well with xmonad.

    - `trayerAboveXmobarEventHook` reliably stacks trayer on top of xmobar and
      below other windows

  * `XMonad.Util.ActionCycle`

    A module providing a simple way to implement "cycling" `X` actions,
    useful for things like alternating toggle-style keybindings.

  * `XMonad.Actions.RotateSome`

    Functions for rotating some elements around the stack while keeping others
    anchored in place. Useful in combination with layouts that dictate window
    visibility based on stack position, such as `XMonad.Layout.LimitWindows`.

    Export `surfaceNext` and `surfacePrev` actions, which treat the focused window
    and any hidden windows as a ring that can be rotated through the focused position.

    Export `rotateSome`, a pure function that rotates some elements around a stack
    while keeping others anchored in place.

  * `XMonad.Actions.Sift`

    Provide `siftUp` and `siftDown` actions, which behave like `swapUp` and `swapDown`
    but handle the wrapping case by exchanging the windows at either end of the stack
    instead of rotating the stack.

  * `XMonad.Hooks.DynamicIcons`

    Dynamically augment workspace names logged to a status bar via DynamicLog
    based on the contents (windows) of the workspace.

  * `XMonad.Hooks.WindowSwallowing`

    HandleEventHooks that implement window swallowing or sublayouting:
    Hide parent windows like terminals when opening other programs (like image viewers) from within them,
    restoring them once the child application closes.

  * `XMonad.Actions.TiledWindowDragging`

    An action that allows you to change the position of windows by dragging them around.

  * `XMonad.Layout.ResizableThreeColumns`

    A layout based on `XMonad.Layout.ThreeColumns` but with each slave window's
    height resizable.

  * `XMonad.Layout.TallMastersCombo`

    A layout combinator that support Shrink, Expand, and IncMasterN just as
    the `Tall` layout, and also support operations of two master windows:
    a main master, which is the original master window;
    a sub master, the first window of the second pane.
    This combinator can be nested, and has a good support for using
    `XMonad.Layout.Tabbed` as a sublayout.

  * `XMonad.Actions.PerWindowKeys`

    Create actions that run on a `Query Bool`, usually associated with
    conditions on a window, basis. Useful for creating bindings that are
    excluded or exclusive for some windows.

  * `XMonad.Util.DynamicScratchpads`

    Declare any window as a scratchpad on the fly. Once declared, the
    scratchpad behaves like `XMonad.Util.NamedScratchpad`.

  * `XMonad.Prompt.Zsh`

    A version of `XMonad.Prompt.Shell` that lets you use completions supplied by
    zsh.

  * `XMonad.Util.ClickableWorkspaces`

    Provides `clickablePP`, which when applied to the `PP` pretty-printer used by
    `XMonad.Hooks.StatusBar.PP`, will make the workspace tags clickable in XMobar
    (for switching focus).

  * `XMonad.Layout.VoidBorders`

    Provides a modifier that semi-permanently (requires manual intervention)
    disables borders for windows from the layout it modifies.

  * `XMonad.Hooks.Focus`

    Extends ManageHook EDSL to work on focused windows and current workspace.

  * `XMonad.Config.LXQt`

    This module provides a config suitable for use with the LXQt desktop
    environment.

  * `XMonad.Prompt.OrgMode`

    A prompt for interacting with [org-mode](https://orgmode.org/).  It
    can be used to quickly save TODOs, NOTEs, and the like with the
    additional capability to schedule/deadline a task, or use the
    primary selection as the contents of the note.

  * `XMonad.Util.ExtensibleConf`

    Extensible and composable configuration for contrib modules. Allows
    contrib modules to store custom configuration values inside `XConfig`.
    This lets them create custom hooks, ensure they hook into xmonad core only
    once, and possibly more.

  * `XMonad.Hooks.Rescreen`

    Custom hooks for screen (xrandr) configuration changes. These can be used
    to restart/reposition status bars or systrays automatically after xrandr,
    as well as to actually invoke xrandr or autorandr when an output is
    (dis)connected.

  * `XMonad.Actions.EasyMotion`

    A new module that allows selection of visible screens using a key chord.
    Inspired by [vim-easymotion](https://github.com/easymotion/vim-easymotion). See the animation
    in the vim-easymotion repo to get some idea of the functionality of this
    EasyMotion module.

### Bug Fixes and Minor Changes

  * Add support for GHC 9.0.1.

  * `XMonad.Actions.WithAll`

    - Added `killOthers`, which kills all unfocused windows on the
      current workspace.

  * `XMonad.Prompt.XMonad`

    - Added `xmonadPromptCT`, which allows you to create an XMonad
      prompt with a custom title.

  * `XMonad.Actions.DynamicWorkspaceGroups`

    - Add support for `XMonad.Actions.TopicSpace` through `viewTopicGroup` and
      `promptTopicGroupView`.

  * `XMonad.Actions.TreeSelect`

    - Fix swapped green/blue in foreground when using Xft.

    - The spawned tree-select window now sets a `WM_CLASS` property.
      This allows other applications, like compositors, to properly
      match on it.

  * `XMonad.Layout.Fullscreen`

    - Add fullscreenSupportBorder which uses smartBorders to remove
      window borders when the window is fullscreen.

  * `XMonad.Config.Mate`

    - Split out the logout dialog and add a shutdown dialog. The default behavior
      remains the same but there are now `mateLogout` and `mateShutdown` actions
      available.

    - Add mod-d keybinding to open the Mate main menu.

  * `XMonad.Actions.DynamicProjects`

    - The `changeProjectDirPrompt` function respects the `complCaseSensitivity` field
      of `XPConfig` when performing directory completion.

    - `modifyProject` is now exported.

  * `XMonad.Layout.WorkspaceDir`

    - The `changeDir` function respects the `complCaseSensitivity` field of `XPConfig`
      when performing directory completion.

    - `Chdir` message is exported, so it's now possible to change the
      directory programmaticaly, not just via a user prompt.

  * `XMonad.Prompt.Directory`

    - Added `directoryMultipleModes'`, like `directoryMultipleModes` with an additional
     `ComplCaseSensitivity` argument.

    - Directory completions are now sorted.

    - The `Dir` constructor now takes an additional `ComplCaseSensitivity`
      argument to indicate whether directory completion is case sensitive.

  * `XMonad.Prompt.FuzzyMatch`

    - `fuzzySort` will now accept cases where the input is not a subsequence of
      every completion.

  * `XMonad.Prompt.Shell`

    - Added `getShellCompl'`, like `getShellCompl` with an additional `ComplCaseSensitivity`
      argument.

    - Added `compgenDirectories` and `compgenFiles` to get the directory/filename completion
      matches returned by the compgen shell builtin.

    - Added `safeDirPrompt`, which is like `safePrompt`, but optimized
      for the use-case of a program that needs a file as an argument.

  * `XMonad.Prompt.Unicode`

    - Reworked internally to call `spawnPipe` (asynchronous) instead of
      `runProcessWithInput` (synchronous), which fixes `typeUnicodePrompt`.

    - Now respects `searchPredicate` and `sorter` from user-supplied `XPConfig`.

  * `XMonad.Hooks.DynamicLog`

    - Added `xmobarProp`, for property-based alternative to `xmobar`.

    - Add the -dock argument to the dzen spawn arguments

    - The API for this module is frozen: this is now a compatibility wrapper.

    - References for this module are updated to point to `X.H.StatusBar` or
      `X.H.StatusBar.PP`

  * `XMonad.Layout.BoringWindows`

    - Added boring-aware `swapUp`, `swapDown`, `siftUp`, and `siftDown` functions.

    - Added `markBoringEverywhere` function, to mark the currently
      focused window boring on all layouts, when using `XMonad.Actions.CopyWindow`.

  * `XMonad.Util.NamedScratchpad`

     - Added two new exported functions to the module:
         - `customRunNamedScratchpadAction`
             (provides the option to customize the `X ()` action the scratchpad is launched by)
         - `spawnHereNamedScratchpadAction`
             (uses `XMonad.Actions.SpawnOn.spawnHere` to initially start the scratchpad on the workspace it was launched on)

     - Deprecated `namedScratchpadFilterOutWorkspace` and
       `namedScratchpadFilterOutWorkspacePP`.  Use
       `XMonad.Util.WorkspaceCompare.filterOutWs` respectively
       `XMonad.Hooks.DynamicLog.filterOutWsPP` instead.

     - Exported the `scratchpadWorkspaceTag`.

     - Added a new logHook `nsHideOnFocusLoss` for hiding scratchpads
       when they lose focus.

  * `XMonad.Prompt.Window`

    - Added `allApplications` function which maps application executable
      names to its underlying window.

    - Added a `WithWindow` constructor to `WindowPrompt` to allow executing
      actions of type `Window -> X ()` on the chosen window.

  * `XMonad.Prompt.WindowBringer`

    - Added `windowAppMap` function which maps application executable
      names to its underlying window.

    - A new field `windowFilter` was added to the config, which allows the user
      to provide a function which will decide whether each window should be
      included in the window bringer menu.

  * `XMonad.Actions.Search`

    - The `hoogle` function now uses the new URL `hoogle.haskell.org`.

    - Added `promptSearchBrowser'` function to only suggest previous searches of
      the selected search engine (instead of all search engines).

  * `XMonad.Layout.MouseResizableTile`

    - When we calculate dragger widths, we first try to get the border width of
      the focused window, before failing over to using the initial `borderWidth`.

  * `XMonad.Actions.CycleRecentWS`

    - Added `cycleRecentNonEmptyWS` function which behaves like `cycleRecentWS`
      but is constrainded to non-empty workspaces.

    - Added `toggleRecentWS` and `toggleRecentNonEmptyWS` functions which toggle
      between the current and most recent workspace, and continue to toggle back
      and forth on repeated presses, rather than cycling through other workspaces.

    - Added `recentWS` function which allows the recency list to be filtered with
      a user-provided predicate.

  * `XMonad.Layout.Hidden`

    - Export `HiddenWindows` type constructor.

    - Export `popHiddenWindow` function restoring a specific window.

  * `XMonad.Hooks.ManageDocks`

    - Export `AvoidStruts` constructor

    - Restored compatibility with pre-0.13 configs by making the startup hook
      unnecessary for correct functioning (strut cache is initialized on-demand).

    - Fixed ignoring of strut updates from override-redirect windows, which is
      default for xmobar.

      Previously, if one wanted xmobar to reposition itself after xrandr
      changes and have xmonad handle that repositioning, one would need to
      configure xmobar with `overrideRedirect = False`, which would disable
      lowering on start and thus cause other problems. This is no longer
      necessary.

  * `XMonad.Hooks.ManageHelpers`

    - Export `doSink`

    - Added `doLower` and `doRaise`

    - Added `shiftToSame` and `clientLeader` which allow a hook to be created
      that shifts a window to the workspace of other windows of the application
      (using either the `WM_CLIENT_LEADER` or `_NET_WM_PID` property).

    - Added `windowTag`

  * `XMonad.Util.EZConfig`

    - Added support for XF86Bluetooth.

  * `XMonad.Util.Loggers`

    - Make `battery` and `loadAvg` distro-independent.

    - Added `logTitleOnScreen`, `logCurrentOnScreen` and `logLayoutOnScreen`
      as screen-specific variants of `logTitle`, `logCurrent` and `logLayout`.

    - Added `logWhenActive` to have loggers active only when a certain
      screen is active.

    - Added `logConst` to log a constant `String`, and `logDefault` (infix: `.|`)
      to combine loggers.

    - Added `logTitles` to log all window titles (focused and unfocused
      ones) on the focused workspace, as well as `logTitlesOnScreen` as
      a screen-specific variant thereof.

  * `XMonad.Layout.Minimize`

    - Export `Minimize` type constructor.

  * `XMonad.Actions.WorkspaceNames`

    - Added `workspaceNamesEwmh` which makes workspace names visible to
      external pagers.

  * `XMonad.Util.PureX`

    - Added `focusWindow` and `focusNth` which don't refresh (and thus
      possibly flicker) when they happen to be a no-op.

    - Added `shiftWin` as a refresh tracking version of `W.shiftWin`.

  * Several `LayoutClass` instances now have an additional `Typeable`
    constraint which may break some advanced configs. The upside is that we
    can now add `Typeable` to `LayoutClass` in `XMonad.Core` and make it
    possible to introspect the current layout and its modifiers.

  * `XMonad.Actions.TopicSpace`

    - `switchTopic` now correctly updates the last used topics.

    - `setLastFocusedTopic` will now check whether we have exceeded the
      `maxTopicHistory` and prune the topic history as necessary, as well as
      cons the given topic onto the list __before__ filtering it.

    - Added `switchNthLastFocusedExclude`, which works like
      `switchNthLastFocused` but is able to exclude certain topics.

    - Added `switchTopicWith`, which works like `switchTopic`, but one is able
      to give `setLastFocusedTopic` a custom filtering function as well.

    - Instead of a hand-rolled history, use the one from
      `XMonad.Hooks.WorkspaceHistory`.

    - Added the screen-aware functions `getLastFocusedTopicsByScreen` and
      `switchNthLastFocusedByScreen`.

  * `XMonad.Hooks.WorkspaceHistory`

    - Added `workspaceHistoryModify` to modify the workspace history with a pure
      function.

    - Added `workspaceHistoryHookExclude` for excluding certain
      workspaces to ever enter the history.

  * `XMonad.Util.DebugWindow`

    - Fixed a bottom in `debugWindow` when used on windows with UTF8 encoded titles.

  * `XMonad.Config.Xfce`

    - Set `terminal` to `xfce4-terminal`.

  * `XMonad.Hooks.WorkspaceCompare`

    - Added `filterOutWs` for workspace filtering.

  * `XMonad.Prompt`

    - Accommodate completion of multiple words even when `alwaysHighlight` is
      enabled.

    - Made the history respect words that were "completed" by `alwaysHighlight`
      upon confirmation of the selection by the user.

    - Fixed a crash when focusing a new window while the prompt was up
      by allowing pointer events to pass through the custom prompt event
      loop.

    - The prompt now cycles through its suggestions if one hits the ends
      of the suggestion list and presses `TAB` again.

    - Added `maxComplColumns` field to `XPConfig`, to limit the number of
      columns in the completion window.

    - Redefine `ComplCaseSensitivity` to a proper sum type as opposed to
      a `newtype` wrapper around `Bool`.

  * `XMonad.Actions.TreeSelect`

    - Fixed a crash when focusing a new window while the tree select
      window was up by allowing pointer events to pass through the
      custom tree select event loop.

  * `XMonad.Layout.NoBorders`

    - Fixed handling of floating window borders in multihead setups that was
      broken since 0.14.

    - Added `OnlyFloat` constructor to `Ambiguity` to unconditionally
      remove all borders on floating windows.

  * `XMonad.Hooks.UrgencyHook`

    - It's now possible to clear urgency of selected windows only using the
      newly exported `clearUrgents'` function. Also, this and `clearUrgents`
      now clear the `_NET_WM_STATE_DEMANDS_ATTENTION` bit as well.

    - Added a variant of `filterUrgencyHook` that takes a generic `Query Bool`
      to select which windows should never be marked urgent.

    - Added `askUrgent` and a `doAskUrgent` manage hook helper for marking
      windows as urgent from inside of xmonad. This can be used as a less
      intrusive action for windows requesting focus.

  * `XMonad.Hooks.ServerMode`

    - To make it easier to use, the `xmonadctl` client is now included in
      `scripts/`.

  * `XMonad.Layout.TrackFloating`

    - Fixed a bug that prevented changing focus on inactive workspaces.

  * `XMonad.Layout.Magnifier`

    - Added `magnifierczOff` and `magnifierczOff'` for custom-size
      magnifiers that start out with magnifying disabled.

    - Added `magnify` as a more general combinator, including the
      ability to postpone magnifying until there are a certain number of
      windows on the workspace.

  * `XMonad.Layout.Renamed`

    - Added `KeepWordsLeft` and `KeepWordsRight` for keeping certain number of
      words in left or right direction in layout description.

  * `XMonad.Hooks.WallpaperSetter`

    - Added `defWPNamesPng`, which works like `defWPNames` but maps
      `ws-name` to `ws-name.png` instead of `ws-name.jpg`.

    - Added `defWPNamesJpg` as an alias to `defWPNames` and deprecated
      the latter.

  * `XMonad.Layout.SubLayouts`

    - Floating windows are no longer moved to the end of the window stack.

  * `XMonad.Layout.BinarySpacePartition`

    - Add the ability to increase/decrease the window size by a custom
      rational number. E.g: `sendMessage $ ExpandTowardsBy L 0.02`

  * `XMonad.Layout.Decoration`

    - The decoration window now sets a `WM_CLASS` property.  This allows
      other applications, like compositors, to properly match on it.

  * `XMonad.Layout.IndependentScreens`

    - Fixed a bug where `marshallPP` always sorted workspace names
      lexically.  This changes the default behaviour of `marshallPP`—the
      given `ppSort` now operates in the _physical_ workspace names.
      The documentation of `marshallSort` contains an example of how to
      get the old behaviour, where `ppSort` operates in virtual names,
      back.

    - Added `workspacesOn` for filtering workspaces on the current screen.

    - Added `withScreen` to specify names for a given single screen.

    - Added new aliases `PhysicalWindowSpace` and `VirtualWindowSpace`
      for a `WindowSpace` for easier to read function signatures.

  * `XMonad.Actions.CopyWindow`

    - Added `copiesPP` to make a `PP` aware of copies of the focused
      window.

  - `XMonad.Actions.CycleWS`

    - Added `:&:`, `:|:` and `Not` data constructors to `WSType` to logically
      combine predicates.

    - Added `hiddenWS`, `emptyWS` and `anyWS` to replace deprecated
      constructors.

    - Added `ingoringWSs` as a `WSType` predicate to skip workspaces having a
      tag in a given list.

  - `XMonad.Actions.DynamicWorkspaceOrder`

    - Added `swapWithCurrent` and `swapOrder` to the list of exported names.

  - `XMonad.Actions.Submap`, `XMonad.Util.Ungrab`:

    - Fixed issue with keyboard/pointer staying grabbed when a blocking action
      like `runProcessWithInput` was invoked.

  - `XMonad.Actions.UpdateFocus`

    - Added `focusUnderPointer`, that updates the focus based on pointer
      position, an inverse of `X.A.UpdatePointer`, which moves the mouse
      pointer to match the focused window). Together these can be used to
      ensure focus stays in sync with mouse.

  - `XMonad.Layout.MultiToggle`

    - Added `isToggleActive` for querying the toggle state of transformers.
      Useful to show the state in a status bar.

  * `XMonad.Layout.Spacing`

    - Removed deprecations for `spacing`, `spacingWithEdge`,
      `smartSpacing`, and `smartSpacingWithEdge`.

  * `XMonad.Actions.DynamicWorkspaces`

    - Fixed a system freeze when using `X.A.CopyWindow.copy` in
      combination with `removeWorkspace`.

## 0.16

### Breaking Changes

  * `XMonad.Layout.Decoration`
    - Added `Theme` record fields for controlling decoration border width for active/inactive/urgent windows.
  * `XMonad.Prompt`

    - Prompt ships a vim-like keymap, see `vimLikeXPKeymap` and
      `vimLikeXPKeymap'`. A reworked event loop supports new vim-like prompt
      actions.
    - Prompt supports dynamic colors. Colors are now specified by the `XPColor`
      type in `XPState` while `XPConfig` colors remain unchanged for backwards
      compatibility.
    - Fixes `showCompletionOnTab`.
    - The behavior of `moveWord` and `moveWord'` has changed; brought in line
      with the documentation and now internally consistent. The old keymaps
      retain the original behavior; see the documentation to do the same your
      XMonad configuration.
  * `XMonad.Util.Invisble`
    - Requires `MonadFail` for `Read` instance

### New Modules

  * `XMonad.Layout.TwoPanePersistent`

    A layout that is like TwoPane but keeps track of the slave window that is
    currently beside the master. In TwoPane, the default behavior when the master
    is focused is to display the next window in the stack on the slave pane. This
    is a problem when a different slave window is selected without changing the stack
    order.

  * `XMonad.Util.ExclusiveScratchpads`

    Named scratchpads that can be mutually exclusive: This new module extends the
    idea of named scratchpads such that you can define "families of scratchpads"
    that are exclusive on the same screen. It also allows to remove this
    constraint of being mutually exclusive with another scratchpad.

  * `XMonad.Actions.Prefix`

    A module that allows the user to use an Emacs-style prefix
    argument (raw or numeric).

### Bug Fixes and Minor Changes

  * `XMonad.Layout.Tabbed`

    tabbedLeft and tabbedRight will set their tabs' height and width according to decoHeight/decoWidth

  * `XMonad.Prompt`

    Added `sorter` to `XPConfig` used to sort the possible completions by how
    well they match the search string (example: `XMonad.Prompt.FuzzyMatch`).

    Fixes a potential bug where an error during prompt execution would
    leave the window open and keep the keyboard grabbed. See issue
    [#180](https://github.com/xmonad/xmonad-contrib/issues/180).

    Fixes [issue #217](https://github.com/xmonad/xmonad-contrib/issues/217), where
    using tab to wrap around the completion rows would fail when maxComplRows is
    restricting the number of rows of output.

  * `XMonad.Prompt.Pass`

    Added 'passOTPPrompt' to support getting OTP type password. This require
    pass-otp (https://github.com/tadfisher/pass-otp) has been setup in the running
    machine.

    Added 'passGenerateAndCopyPrompt', which both generates a new password and
    copies it to the clipboard.  These two actions are commonly desirable to
    take together, e.g. when establishing a new account.

    Made password prompts traverse symlinks when gathering password names for
    autocomplete.

  * `XMonad.Actions.DynamicProjects`

    Make the input directory read from the prompt in `DynamicProjects`
    absolute wrt the current directory.

    Before this, the directory set by the prompt was treated like a relative
    directory. This means that when you switch from a project with directory
    `foo` into a project with directory `bar`, xmonad actually tries to `cd`
    into `foo/bar`, instead of `~/bar` as expected.

  * `XMonad.Actions.DynamicWorkspaceOrder`

    Add a version of `withNthWorkspace` that takes a `[WorkspaceId] ->
    [WorkspaceId]` transformation to apply over the list of workspace tags
    resulting from the dynamic order.

  * `XMonad.Actions.GroupNavigation`

    Add a utility function `isOnAnyVisibleWS :: Query Bool` to allow easy
    cycling between all windows on all visible workspaces.


  * `XMonad.Hooks.WallpaperSetter`

    Preserve the aspect ratio of wallpapers that xmonad sets. When previous
    versions would distort images to fit the screen size, it will now find a
    best fit by cropping instead.

  * `XMonad.Util.Themes`

    Add adwaitaTheme and adwaitaDarkTheme to match their respective
    GTK themes.

  * 'XMonad.Layout.BinarySpacePartition'

    Add a new `SplitShiftDirectional` message that allows moving windows by
    splitting its neighbours.

  * `XMonad.Prompt.FuzzyMatch`

    Make fuzzy sort show shorter strings first.

## 0.15

### Breaking Changes

  * `XMonad.Layout.Groups` & `XMonad.Layout.Groups.Helpers`
    The layout will no longer perform refreshes inside of its message handling.
    If you have been relying on it to in your xmonad.hs, you will need to start
    sending its messages in a manner that properly handles refreshing, e.g. with
    `sendMessage`.

### New Modules

  * `XMonad.Util.Purex`

    Unlike the opaque `IO` actions that `X` actions can wrap, regular reads from
    the `XConf` and modifications to the `XState` are fundamentally pure --
    contrary to the current treatment of such actions in most xmonad code. Pure
    modifications to the `WindowSet` can be readily composed, but due to the
    need for those modifications to be properly handled by `windows`, other pure
    changes to the `XState` cannot be interleaved with those changes to the
    `WindowSet` without superfluous refreshes, hence breaking composability.

    This module aims to rectify that situation by drawing attention to it and
    providing `PureX`: a pure type with the same monadic interface to state as
    `X`. The `XLike` typeclass enables writing actions generic over the two
    monads; if pure, existing `X` actions can be generalised with only a change
    to the type signature. Various other utilities are provided, in particular
    the `defile` function which is needed by end-users.

### Bug Fixes and Minor Changes

  * Add support for GHC 8.6.1.

  * `XMonad.Actions.MessageHandling`
    Refresh-performing functions updated to better reflect the new `sendMessage`.

## 0.14

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

  * `XMonad.Actions.PhysicalScreens`

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
    `XMonad.Actions.PhysicalScreens` (see changelog of this module for more information)

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

  * `XMonad.Actions.MessageFeedback`

    - Follow the naming conventions of `XMonad.Operations`. Functions returning
      `X ()` are named regularly (previously these ended in underscore) while
      those returning `X Bool` are suffixed with an uppercase 'B'.
    - Provide all `X Bool` and `SomeMessage` variations for `sendMessage` and
      `sendMessageWithNoRefresh`, not just `sendMessageWithNoRefreshToCurrent`
      (renamed from `send`).
    - The new `tryInOrderB` and `tryMessageB` functions accept a parameter of
      type `SomeMessage -> X Bool`, which means you are no longer constrained
      to the behavior of the `sendMessageWithNoRefreshToCurrent` dispatcher.
    - The `send*Messages*` family of funtions allows for sequencing arbitrary
      sets of messages with minimal refresh. It makes little sense for these
      functions to support custom message dispatchers.
    - Remain backwards compatible. Maintain deprecated aliases of all renamed
      functions:
      - `send`          -> `sendMessageWithNoRefreshToCurrentB`
      - `sendSM`        -> `sendSomeMessageWithNoRefreshToCurrentB`
      - `sendSM_`       -> `sendSomeMessageWithNoRefreshToCurrent`
      - `tryInOrder`    -> `tryInOrderWithNoRefreshToCurrentB`
      - `tryInOrder_`   -> `tryInOrderWithNoRefreshToCurrent`
      - `tryMessage`    -> `tryMessageWithNoRefreshToCurrentB`
      - `tryMessage_`   -> `tryMessageWithNoRefreshToCurrent`

### New Modules

  * `XMonad.Layout.MultiToggle.TabBarDecoration`

    Provides a simple transformer for use with `XMonad.Layout.MultiToggle` to
    dynamically toggle `XMonad.Layout.TabBarDecoration`.

  * `XMonad.Hooks.RefocusLast`

    Provides hooks and actions that keep track of recently focused windows on a
    per workspace basis and automatically refocus the last window on loss of the
    current (if appropriate as determined by user specified criteria).

  * `XMonad.Layout.StateFull`

    Provides `StateFull`: a stateful form of `Full` that does not misbehave when
    floats are focused, and the `FocusTracking` layout transformer by means of
    which `StateFull` is implemented. `FocusTracking` simply holds onto the last
    true focus it was given and continues to use it as the focus for the
    transformed layout until it sees another. It can be used to improve the
    behaviour of a child layout that has not been given the focused window.

  * `XMonad.Actions.SwapPromote`

    Module for tracking master window history per workspace, and associated
    functions for manipulating the stack using such history.

  * `XMonad.Actions.CycleWorkspaceByScreen`

    A new module that allows cycling through previously viewed workspaces in the
    order they were viewed most recently on the screen where cycling is taking
    place.

    Also provides the `repeatableAction` helper function which can be used to
    build actions that can be repeated while a modifier key is held down.

  * `XMonad.Prompt.FuzzyMatch`

    Provides a predicate `fuzzyMatch` that is much more lenient in matching
    completions in `XMonad.Prompt` than the default prefix match.  Also provides
    a function `fuzzySort` that allows sorting the fuzzy matches by "how well"
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

  * `XMonad.Hooks.FadeWindows`

    Added support for GHC version 8.4.x by adding a Semigroup instance for
    Monoids

  * `XMonad.Hooks.WallpaperSetter`

    Added support for GHC version 8.4.x by adding a Semigroup instance for
    Monoids

  * `XMonad.Hooks.Mosaic`

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

    - Make type of ManageHook combinators more general.
    - New manage hook `doSink` for sinking windows (as upposed to the `doFloat` manage hook)

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
    - New function `passEditPrompt` for editing a password from the
      store.
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

  * `XMonad.Config.Azerty`

    * Added `belgianConfig` and `belgianKeys` to support Belgian AZERTY
      keyboards, which are slightly different from the French ones in the top
      row.

## 0.13 (February 10, 2017)

### Breaking Changes

  * The type of `completionKey` (of `XPConfig` record) has been
    changed from `KeySym` to `(KeyMask, KeySym)`. The default value
    for this is still bound to the `Tab` key.

  * New constructor `CenteredAt Rational Rational` added for
    `XMonad.Prompt.XPPosition`.

  * `XMonad.Prompt` now stores its history file in the XMonad cache
    directory in a file named `prompt-history`.

  * `XMonad.Hooks.ManageDocks` now requires an additional startup hook to be
    added to configuration in addition to the other 3 hooks, otherwise docks
    started before xmonad are covered by windows. It's recommended to use the
    newly introduced `docks` function to add all necessary hooks to xmonad
    config.

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

  * `XMonad.Hooks.ManageDocks`

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
