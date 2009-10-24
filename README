  xmonad-contrib : third party extensions to the xmonad window manager

                           http://xmonad.org

    You need the ghc compiler and xmonad window manager installed in
    order to use these extensions.

    For installation and configuration instructions, please see the
    xmonad website, the documents included with the xmonad source
    distribution, and online haddock documentation:

    http://www.xmonad.org/xmonad-docs

------------------------------------------------------------------------

Changelogs

    For a list of changes since the 0.8.x releases, see:

http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8

------------------------------------------------------------------------

Updates to XMonadContrib-0.9 that may Require Changes to ~/.xmonad/xmonad.hs

    Please see the Changelogs and xmonad-contrib haddock documentation
    links for further details regarding the following changes.

    * XMonad.Hooks.EwmhDesktops no longer uses layoutHook, the
    ewmhDesktopsLayout modifier has been removed from xmonad-contrib. It
    uses logHook, handleEventHook, and startupHook instead and provides
    a convenient function 'ewmh' to add EWMH support to a defaultConfig.

    * Most DynamicLog users can continue with configs unchanged, but users
    of the quickbar functions 'xmobar' or 'dzen' will need to change
    xmonad.hs: their types have changed to allow easier composition with
    other XConfig modifiers. The 'dynamicLogDzen' and 'dynamicLogXmobar'
    functions have been removed.

    * WindowGo or safeSpawn users may need to change command lines due to
    safeSpawn changes.

    * People explicitly referencing the "SP" scratchpad workspace should
    change it to "NSP" which is also used by the new Util.NamedScratchpad.

    * (Optional) People who explicitly use swapMaster in key or mouse
    bindings should change it to shiftMaster. It's the current default
    used where swapMaster had been used previously. It works better than
    swapMaster when using floating and tiled windows together on the
    same workspace.

------------------------------------------------------------------------

Getting or updating XMonadContrib

    latest release: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/xmonad-contrib

    darcs version:  darcs get http://code.haskell.org/XMonadContrib

    (To use darcs xmonad-contrib you must also use the darcs version
    of xmonad.)

------------------------------------------------------------------------

Contributing

    Haskell code contributed to this repo should live under the
    appropriate subdivision of the 'XMonad.' namespace (currently
    includes Actions, Config, Hooks, Layout, Prompt, and Util). For
    example, to use the Grid layout, one would import:

        XMonad.Layout.Grid

    For further details, see the documentation for the
    XMonad.Doc.Developing module and http://xmonad.org website.

------------------------------------------------------------------------

Code submitted to the contrib repo is licensed under the same license as
xmonad itself, with copyright held by the authors.

------------------------------------------------------------------------
