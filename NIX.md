# `nix` integration for XMonad

## Customizing the `nix-shell`

It's possible to use a file `develop.nix` to customize the `devShell`
provided by the flake.  This is useful if e.g. you want to have the
`haskell-language-server` or other developer tools in the shell properly
configured (correct GHC versions, and the like).

Here is an example `develop.nix` for `haskell-language-server`:

``` nix
pkgs: devInputs: devInputs // {
  nativeBuildInputs = with pkgs.haskellPackages;
    [ cabal-install hlint ghcid ormolu implicit-hie haskell-language-server ];
}
```
