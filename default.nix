{ sources ? import ./nix/sources.nix {}
, haskellNix ? import sources.haskellNix {}
, pkgsSrc ? import haskellNix.sources.nixpkgs-2211 # nixpkgs-unstable
, pkgs ? pkgsSrc (haskellNix.nixpkgsArgs // { })
}:
let
  mkHackage = import sources.haskell-nix-extra-hackage { inherit pkgs; };

  # Docs: https://github.com/ilyakooo0/haskell-nix-extra-hackage/blob/master/README.md#how-to-use-it
  hsPkgs = pkgs.haskell-nix.cabalProject ({
    src = ./.;
    index-state = "2021-11-22T00:00:00Z";
    compiler-nix-name = "ghc8107";
  });
in
hsPkgs