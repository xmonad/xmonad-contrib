<p align="center">
  <a href="https://xmonad.org/">
    <img alt="XMonad logo" src="https://xmonad.org/images/logo-wrapped.svg" height=150>
  </a>
</p>
<p align="center">
  <a href="https://hackage.haskell.org/package/xmonad-contrib">
    <img alt="Hackage" src="https://img.shields.io/hackage/v/xmonad-contrib?logo=haskell">
  </a>
  <a href="https://github.com/xmonad/xmonad-contrib/blob/readme/LICENSE">
    <img alt="License" src="https://img.shields.io/github/license/xmonad/xmonad-contrib">
  </a>
  <a href="https://haskell.org/">
    <img alt="Made in Haskell" src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell">
  </a>
  <br>
  <a href="https://github.com/xmonad/xmonad-contrib/actions/workflows/stack.yml">
    <img alt="Stack" src="https://img.shields.io/github/workflow/status/xmonad/xmonad-contrib/Stack?label=Stack&logo=githubactions&logoColor=white">
  </a>
  <a href="https://github.com/xmonad/xmonad-contrib/actions/workflows/haskell-ci.yml">
    <img alt="Cabal" src="https://img.shields.io/github/workflow/status/xmonad/xmonad-contrib/Haskell-CI?label=Cabal&logo=githubactions&logoColor=white">
  </a>
  <a href="https://github.com/xmonad/xmonad-contrib/actions/workflows/nix.yml">
    <img alt="Nix" src="https://img.shields.io/github/workflow/status/xmonad/xmonad-contrib/Nix?label=Nix&logo=githubactions&logoColor=white">
  </a>
  <br>
  <a href="https://github.com/sponsors/xmonad">
    <img alt="GitHub Sponsors" src="https://img.shields.io/github/sponsors/xmonad?label=GitHub%20Sponsors&logo=githubsponsors">
  </a>
  <a href="https://opencollective.com/xmonad">
    <img alt="Open Collective" src="https://img.shields.io/opencollective/all/xmonad?label=Open%20Collective&logo=opencollective">
  </a>
</p>

# xmonad-contrib

**Community-maintained extensions for the [XMonad][web:xmonad] window manager.**

[xmonad core][gh:xmonad] is minimal, stable, yet extensible.
[xmonad-contrib][gh:xmonad-contrib] is home to hundreds of additional tiling
algorithms and extension modules. The two combined make for a powerful X11
window-manager with endless customization possibilities. They are, quite
literally, libraries for creating your own window manager.

[web:xmonad]: https://xmonad.org/
[gh:xmonad]: https://github.com/xmonad/xmonad
[gh:xmonad-contrib]: https://github.com/xmonad/xmonad-contrib

## Installation

For installation and configuration instructions, please see:

 * [downloading and installing xmonad](https://xmonad.org/download.html)
 * [installing latest xmonad snapshot from git](https://xmonad.org/INSTALL.html)
 * [configuring xmonad](https://xmonad.org/TUTORIAL.html)

## Contributing

Haskell code contributed to this repo should live under the
appropriate subdivision of the `XMonad` namespace (currently includes
`Actions`, `Config`, `Hooks`, `Layout`, `Prompt`, and `Util`). For
example, to use the Grid layout, one would import:

    XMonad.Layout.Grid

For further details, see the [documentation for the `XMonad.Doc.Developing`
module][doc:developing], XMonad's [CONTRIBUTING][gh:xmonad:contributing] and
the [xmonad website][web:xmonad].

[gh:xmonad:contributing]: https://github.com/xmonad/xmonad/blob/master/CONTRIBUTING.md
[doc:developing]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html

## License

Code submitted to the xmonad-contrib repo is licensed under the same license
as xmonad core itself, with copyright held by the authors.
