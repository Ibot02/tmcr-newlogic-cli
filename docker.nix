{ pkgs ? import <nixpkgs> {} }:
let
  bin = pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callCabal2nix "tmcr-newlogic-cli" ./. {
    tmcr-newlogic-lib = pkgs.haskellPackages.callCabal2nix "tmcr-newlogic-lib" (pkgs.fetchFromGitHub {
      owner = "Ibot02";
      repo = "tmcr-newlogic-lib";
      rev = "0e00960d9620e90e4e0a9507bc1c00cdbf4d9b33";
      sha256 = "0gpq0x9vp7fqmbsws1y0lx1zysfsxcv9yx64g9iib7ahxaf6i1qp";
    }) {};
  });
in pkgs.dockerTools.buildLayeredImage {
  name = "tmcr-newlogic-cli";
  tag = "latest";
  contents = [ bin ];
  config.Entrypoint = [ "${bin}/bin/tmcr-newlogic-cli" ];
}
