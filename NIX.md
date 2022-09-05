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

## Selecting a Compiler

A `comp.nix` file can be used to set the compiler used for `nix build` etc. E.g.

```nix
{ compiler = "ghc924"; }
```

Note that you must `git add comp.nix`, or it will be invisible to the flake.

There is also a `prefix` option (see documentation below) but it doesn't really
work in this context, since the xmonad flakes don't see the effects of your
system overlays. Instead try the `--override-input` flag, e.g.

```sh
$ nix develop . --override-input nixpkgs 'github:NixOS/nixpkgs/nixos-unstable'
```

## NixOS Modules

The core and contrib flakes provide NixOS configuration modules.
You can bring them into your system flake like so:

```nix
{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-<version>;
    # The xmonad-contrib flake depends upon and re-exports from the xmonad
    # flake. As such, you don't need to use the latter directly. If you wish to
    # use /only/ the xmonad flake, you should beware that the version of
    # contrib you get from nixpkgs might not build against it.
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };

  outputs = { self, nixpkgs, xmonad-contrib }: {
    nixosConfigurations.<hostname> = nixpkgs.lib.nixosSystem rec {
      system = <system>;
      # NixOS module composition is /not/ commutative; order matters.
      # To avoid issues, add `xmonad-contrib.nixosModules` after your standard
      # configuration, but before `modernise` or any module overlaying in a
      # "prefix".
      modules = [
        ./configuration.nix
        ./hardware-configuration.nix
        <myMiscConfigModule>
      ] ++ xmonad-contrib.nixosModules ++ [
        # `modernise` replaces the standard xmonad module and wrapper script
        # with those from unstable. This is currently a necessary workaround to
        # make Mod-q recompilation work.
        xmonad-contrib.modernise.${system}
        <myPrefixModule>
      ];
    };
  };
}
```

Note that `<thing>` should be replaced with a user-supplied `thing`.
`<version>`, `<hostname>` and `<system>` are necessary, while
` <myMiscConfigModule>` and `<myPrefixModule>` are entirely optional.

Having brought in `xmonad-contrib.nixosModules`, you can then set the provided
options in your `configuration.nix` under `flake`:

```nix
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    flake = {
      enable = true;
    # prefix = "unstable";
      compiler = "ghc921";
    };
  };
```

This will use core and contrib from git for your system xmonad, building your
config with the compiler of your choice.

With the flake enabled, the `xmonad.haskellPackages` option is not used
directly, and is instead set by the `flake.compiler` option. When `compiler` is
unset, the default `pkgs.haskellPackages` is used.

The `prefix` option is used if you wish to select your haskell packages from
within, e.g., unstable overlaid into `pkgs` as `pkgs.unstable`.

See the flakes themselves and nix flake documentation for full detail.
