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
      rev = "f8abe1b3792f9ff38c1c4b0854a31fbb91a9ebe9";
      sha256 = "s7HZLCTJn/Th+nmnDq3N4DanwI/b6gEVGa6fprZYYc8=";
    }) {};
  });
in pkgs.dockerTools.buildLayeredImage {
  name = "tmcr-newlogic-cli";
  tag = "latest";
  contents = [ bin ];
  config.Entrypoint = [ "${bin}/bin/tmcr-newlogic-cli" ];
}
