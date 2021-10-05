# This file is maintained by @IvanMalison (github)
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:IvanMalison/gitignore.nix/master;
    xmonad.url = github:xmonad/xmonad;
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, xmonad }:
  let
    fn = prev: (old: {
        overrides =
        (prev.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            xmonad = hself.callCabal2nix "xmonad-contrib" (git-ignore-nix.gitIgnoreSource ./.) { };
          })
        );
      });
    overlay = final: prev: {
      ghcWithHoogle = prev.ghcWithHoogle.override (fn prev);
      ghcWithPackages = prev.ghcWithPackages.override (fn prev);
      haskellPackages = prev.haskellPackages.override (fn prev);
    };
    overlays = [ overlay ];
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
