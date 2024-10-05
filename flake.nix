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

          shell.tools = {
            cabal = {};
            cabal-gild = {};
            fourmolu = {};
            hlint = {};
            haskell-language-server = {};
          };

          shell.buildInputs = with pkgs; [
            nixpkgs-fmt
          ];
        };


        flake = project.flake { };

      in
        pkgs.lib.recursiveUpdate flake {
          packages.default = flake.packages."obloxious:exe:obloxious";
        });
}
