{
  description = "Hydra demo";

  inputs = {
    # based on https://github.com/input-output-hk/hydra-poc/blob/0.5.0/default.nix

    haskellNix.url = "github:input-output-hk/haskell.nix/28dbf2f4bd32a4fbd1a2e9de45d02ad977b062d9";
    iohkNix.url = "github:input-output-hk/iohk-nix/62d853d3216083ecadc8e7f192498bebad4eee76";
    nixpkgs.follows = "haskellNix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, haskellNix, iohkNix, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        overlays = [
          haskellNix.overlay

          # needed for cardano-api which uses a patched libsodium  
          iohkNix.overlays.crypto

          (final: prev: {
            hydraDemoProject = final.haskell-nix.project' {
              compiler-nix-name = "ghc8107";

              src = final.haskell-nix.haskellLib.cleanGit {
                name = "hydra-demo";
                src = ./.;
              };

              modules = [{
                # https://github.com/input-output-hk/iohk-nix/pull/488
                packages.cardano-crypto-class.components.library.pkgconfig = final.lib.mkForce [ [ final.libsodium-vrf ] ];
                packages.cardano-crypto-praos.components.library.pkgconfig = final.lib.mkForce [ [ final.libsodium-vrf ] ];
              }];

              shell = {
                nativeBuildInputs = with final; [
                  nixpkgs-fmt
                  git
                ];

                tools = {
                  cabal = { };
                  cabal-fmt = { };
                  fourmolu = "0.4.0.0";
                  hlint = { };
                };
              };
            };
          })
        ];

        flake = pkgs.hydraDemoProject.flake { };

      in
      {
        flake = flake // {
          defaultPackage = flake.packages."hydra-demo:exe:hydra-rps-game";
        };
        packages = self.flake.${system}.packages;
        checks = self.flake.${system}.checks;
        check = pkgs.runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out";
        apps = self.flake.${system}.apps;
        devShell = self.flake.${system}.devShell;
      });
}