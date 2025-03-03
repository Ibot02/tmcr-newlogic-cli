let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
    tmcr-newlogic-lib = ../lib;
  };
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        ghcid
        haskell-language-server
      ]);
}
