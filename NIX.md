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

## NixOS Modules

The core and contrib flakes provide NixOS configuration modules.
You can bring them into your system flake like so:

```nix
{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-<version>;
    xmonad.url = github:xmonad/xmonad;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };

  outputs = { self, nixpkgs, xmonad, xmonad-contrib }: {
    nixosConfigurations.<hostname> = nixpkgs.lib.nixosSystem rec {
      system = <arch>;
      modules = [
        ./configuration.nix
        xmonad.nixosModule
        xmonad-contrib.nixosModule
      ];
    };
  };
}
```

Then you can set the provided options in your `configuration.nix` under `flake`:

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

With the flake enabled, the `xmonad.haskellPackages` option is not used directly,
and is instead set by the `flake.compiler` option. When `compiler` is unset,
the default `pkgs.haskellPackages` is used.

The `prefix` option is used if you wish to select your haskell packages from
within, e.g., unstable overlaid into `pkgs` as `pkgs.unstable`.

See the flakes themselves and nix flake documentation for full detail.
