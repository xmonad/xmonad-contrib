# This file is maintained by @IvanMalison and @LSLeary (github)
# See NIX.md for an overview of module usage.
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    xmonad.url = github:xmonad/xmonad;
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, xmonad }:
  with xmonad.lib;
  let
    hoverlay = final: prev: hself: hsuper: {
      xmonad-contrib = hself.callCabal2nix "xmonad-contrib"
        (git-ignore-nix.lib.gitignoreSource ./.) { };
    };
    overlay = fromHOL hoverlay { };
    overlays = xmonad.overlays ++ [ overlay ];
    nixosModule = { config, lib, ... }: with lib;
      let
        cfg = config.services.xserver.windowManager.xmonad;
        comp = { inherit (cfg.flake) prefix compiler; };
      in {
        config = mkIf (cfg.flake.enable && cfg.enableContribAndExtras) {
          nixpkgs.overlays = [ (fromHOL hoverlay comp) ];
        };
      };
    nixosModules = xmonad.nixosModules ++ [ nixosModule ];
  in flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system overlays; };
    modifyDevShell =
      if builtins.pathExists ./develop.nix
      then import ./develop.nix
      else _: x: x;
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor (modifyDevShell pkgs {
      packages = p: [ p.xmonad-contrib ];
      nativeBuildInputs = [ pkgs.cabal-install ];
    });
    defaultPackage = pkgs.haskellPackages.xmonad-contrib;
  }) // { inherit hoverlay overlay overlays nixosModule nixosModules; } ;
}
