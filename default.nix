{ sources ? import ./nix/sources.nix { system = "x86_64-linux"; }
, haskellNix ? import sources.haskellNix {}
, pkgsSrc ? import haskellNix.sources.nixpkgs
, pkgs ? pkgsSrc (haskellNix.nixpkgsArgs // { })
}:
let
  mkHackage = import sources.haskell-nix-extra-hackage { inherit pkgs; };

  # Docs: https://github.com/ilyakooo0/haskell-nix-extra-hackage/blob/master/README.md#how-to-use-it
  hsPkgs = pkgs.haskell-nix.cabalProject ({
    src = ./.;
    index-state = "2023-02-18T00:00:00Z";
    compiler-nix-name = "ghc902";
  });
in
hsPkgs