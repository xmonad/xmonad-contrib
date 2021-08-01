# This file is maintained by @IvanMalison (github)
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:IvanMalison/gitignore.nix/master;
    xmonad.url = github:xmonad/xmonad;
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, xmonad }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          xmonad-contrib =
            hself.callCabal2nix "xmonad-contrib" (git-ignore-nix.gitIgnoreSource ./.) { };
        });
      });
    };
    overlays = xmonad.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.xmonad-contrib ];
      nativeBuildInputs = [ pkgs.cabal-install ];
    };
    defaultPackage = pkgs.haskellPackages.xmonad-contrib;
  }) // { inherit overlay overlays; } ;
}
