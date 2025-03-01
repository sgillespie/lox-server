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
            dist =
              let
                mkDist = pkg: platform:
                  let
                    env = { buildInputs = [ pkg pkgs.zip ]; };
                    dist = "lox-server-${pkg.version}-${platform}";
                    packageCmd =
                      if pkgs.lib.hasPrefix "windows" platform then
                        # Produce a zip file on windows
                        ''zip -r "$out/${dist}.zip" .''
                      else
                        # Produce a tar archive on unix-likes
                        ''tar czf "$out/${dist}.tar.gz" .'';
                  in
                    pkgs.runCommand
                      "lox-server"
                      env
                      ''
                        mkdir -p $out release
                        cd release
                        cp -r ${pkg}/bin/* .
                        ${packageCmd}
                      '';
              in
                pkgs.lib.optionalAttrs (system == "x86_64-linux") {
                  x86_64-linux =
                    mkDist
                      flake.packages."x86_64-unknown-linux-musl:lox-server:exe:lox-server"
                      "linux-x86_64";
                  aarch64-linux =
                    mkDist
                      flake.packages."aarch64-unknown-linux-musl:lox-server:exe:lox-server"
                      "linux-aarch64";
                  x86_64-windows =
                    mkDist
                      flake.packages."x86_64-w64-mingw32:lox-server:exe:lox-server"
                      "windows-x86_64";
                } // pkgs.lib.optionalAttrs (system == "aarch64-darwin") {
                  aarch64-macos =
                    mkDist
                      flake.packages."lox-server:exe:lox-server"
                      "macos-aarch64";
                } // pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
                  x86_64-macos =
                    mkDist
                      flake.packages."lox-server:exe:lox-server"
                      "macos-x86_64";
                  aarch64-macos =
                    mkDist
                      flake.packages."aarch64-apple-darwin:lox-server:exe:lox-server"
                      "macos-aarch64";
                };
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
