# xmonad-contrib: Third Party Extensions to the xmonad Window Manager

![Tests](https://github.com/xmonad/xmonad-contrib/workflows/Tests/badge.svg)
[![Open Source Helpers](https://www.codetriage.com/xmonad/xmonad-contrib/badges/users.svg)](https://www.codetriage.com/xmonad/xmonad-contrib)

You need the ghc compiler and xmonad window manager installed in
order to use these extensions.

For installation and configuration instructions, please see the
[xmonad website][xmonad], the documents included with the
[xmonad source distribution][xmonad-git], and the
[online haddock documentation][xmonad-docs].

## Getting or Updating XMonadContrib

  * Latest release: <https://hackage.haskell.org/package/xmonad-contrib>

  * Git version: <https://github.com/xmonad/xmonad-contrib>

(To use git xmonad-contrib you must also use the
[git version of xmonad][xmonad-git].)

## Contributing

Haskell code contributed to this repo should live under the
appropriate subdivision of the `XMonad` namespace (currently includes
`Actions`, `Config`, `Hooks`, `Layout`, `Prompt`, and `Util`). For
example, to use the Grid layout, one would import:

    XMonad.Layout.Grid

For further details, see the [documentation][developing] for the
`XMonad.Doc.Developing` module, XMonad's [CONTRIBUTING.md](https://github.com/xmonad/xmonad/blob/master/CONTRIBUTING.md)  and the [xmonad website][xmonad].

## License

Code submitted to the contrib repo is licensed under the same license as
xmonad itself, with copyright held by the authors.
 
[xmonad]: http://xmonad.org
[xmonad-git]: https://github.com/xmonad/xmonad
[xmonad-docs]: http://hackage.haskell.org/package/xmonad
[developing]: http://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Doc-Developing.html
