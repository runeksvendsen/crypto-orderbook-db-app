{ name ? "crypto-orderbook-service"
}:
with (import ./pkgs.nix);
let exes = import ../default.nix;
in pkgs.dockerTools.buildImage {
  name = name;
  copyToRoot = exes;
  config = {
    Cmd = [ "${exes}/crypto-orderbook-service" ];
  };
}
