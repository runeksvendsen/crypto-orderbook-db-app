{ sources ? import ./nix/sources.nix {}
, haskellNix ? import sources.haskellNix {}
, pkgsSrc ? import haskellNix.sources.nixpkgs-2211 # nixpkgs-unstable
, pkgs ? pkgsSrc (haskellNix.nixpkgsArgs // { })
, name ? "crypto-orderbook-service"
}:
let exes = import ../default.nix;
in pkgs.dockerTools.buildImage {
  name = name;
  copyToRoot = exes;
  config = {
    Cmd = [ "${exes}/crypto-orderbook-service" ];
  };
}
