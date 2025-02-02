{ pkgs ? import (
  builtins.fetchGit {
  name = "nixpkgs-unstable-2025-02-02";
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/heads/nixpkgs-unstable";
  # Commit hash for nixpkgs-unstable as of 2025-02-02
  rev = "9189ac18287c599860e878e905da550aa6dec1cd";
}) {} }:
let
  bin = pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callCabal2nix "tmcr-newlogic-cli" ./. {
    tmcr-newlogic-lib = pkgs.haskellPackages.callCabal2nix "tmcr-newlogic-lib" (pkgs.fetchFromGitHub {
      owner = "Ibot02";
      repo = "tmcr-newlogic-lib";
      rev = "3e2979564780bd2497c01577faf6de96dbc378d3";
      sha256 = "9kMETUrM2sOKRYQ87Se1ctvfy1hP5mm8m4f7mLPqz00=";
    }) {};
  });
in pkgs.dockerTools.buildLayeredImage {
  name = "tmcr-newlogic-cli";
  tag = "latest";
  contents = [ bin ];
  config.Entrypoint = [ "${bin}/bin/tmcr-newlogic-cli" ];
}
