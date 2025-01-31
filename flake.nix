{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }@attrs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc96";

          shell = {
            tools = {
              cabal = {};
              cabal-gild = {};
              fourmolu = {};
              hlint = {};
              haskell-language-server = {};
            };

            buildInputs = with pkgs; [
              nixpkgs-fmt
            ];

            # No need to cross compile for development shell
            crossPlatforms = _: [];
          };

          crossPlatforms = crossPkgs: with crossPkgs;
            pkgs.lib.optionals (system == "x86_64-linux") [
              musl64
              aarch64-multiplatform-musl
              mingwW64
            ] ++ pkgs.lib.optionals (system == "x86_64-darwin") [
              aarch64-darwin
            ];
        };


        flake = project.flake { };

      in
        pkgs.lib.recursiveUpdate flake {
          packages = {
            default = flake.packages."lox-server:exe:lox-server";
          };

          legacyPkgs = pkgs;
        });

  nixConfig = {
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "sgillespie.cachix.org-1:Zgif/WHW2IzHqbMb1z56cMmV5tLAA+zW9d5iB5w/VU4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    substituters = [
      "https://cache.nixos.org/"
      "https://cache.iog.io"
      "https://sgillespie.cachix.org"
    ];

    allow-import-from-derivation = "true";
    experimental-features = ["nix-command flakes"];
  };
}
