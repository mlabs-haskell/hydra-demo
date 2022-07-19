{
  description = "Hydra demo";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]Hydra-Demo \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {

    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/b6f2f3cef01a399376064194fd96711a5bdba4a7";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/b425b51a80f57cadccff5eaf9443bf492b2c51d6";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/cc246841581f1bf445d2dfbd16fd2bb9f4a66389";
      flake = false; # we need it to be available in shell
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/533aec85c1ca05c7d171da44b89341fb736ecfe5";
      flake = false;
    };
    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/40c97e8cd75821d8713b6ed50c87cafc47b51f1c";
      flake = false;
    };
    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url = "github:input-output-hk/hedgehog-extras/967d79533c21e33387d0227a5f6cc185203fe658";
      flake = false;
    };
    hw-aeson = {
      url = "github:haskell-works/hw-aeson/d99d2f3e39a287607418ae605b132a3deb2b753f";
      flake = false;
    };
    hydra-poc = {
      url = "github:input-output-hk/hydra-poc/539cc6d1c0ca2f57023684f5dcb4029db5fdf981";
      flake = false;
    };
    hysterical-screams = {
      url = "github:raduom/hysterical-screams/f3bbd38a19f99de5c8ddc650c94330b2d09a865b";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
      flake = false;
    };
    io-sim = {
      url =
        "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/5f882881fc1279a9ab8699721248162e2881c6ca";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/f680ac6979e069fcc013e4389ee607ff5fa6672f";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/c2b310968d0915e2af0ea4680186b41ad88ffbe9";
      flake = false;
    };
    typed-protocols = {
      url =
        "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, haskell-nix, iohk-nix, nixpkgs, flake-utils, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      cabalProjectLocal = ''
        allow-newer: *:aeson, size-based:template-haskell
        constraints: bimap >= 0.4.0, hedgehog >= 1.0
      '';

      deferPluginErrors = true;

      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-script-utils.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          })
        ];

      extraSources = [
        {
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "cardano-crypto-tests"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/alonzo/test-suite"
            "eras/babbage/impl"
            "eras/babbage/test-suite"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley/test-suite"
            "eras/shelley-ma/impl"
            "eras/shelley-ma/test-suite"
            "libs/cardano-data"
            "libs/cardano-ledger-core"
            "libs/cardano-ledger-pretty"
            "libs/cardano-ledger-test"
            "libs/cardano-protocol-tpraos"
            "libs/vector-map"
            "libs/non-integral"
            "libs/set-algebra"
            "libs/small-steps"
            "libs/small-steps-test"
          ];
        }
        {
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-cli"
            "cardano-git-rev"
            "cardano-node"
            "cardano-submit-api"
            "cardano-testnet"
            "trace-dispatcher"
            "trace-forward"
            "trace-resources"
          ];
        }
        {
          src = inputs.cardano-config;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
          src = inputs.cardano-wallet;
          subdirs = [
            # "lib/cli"
            "lib/core"
            # "lib/core-integration"
            "lib/dbvar"
            # "lib/launcher"
            "lib/numeric"
            # "lib/shelley"
            "lib/strict-non-empty-containers"
            "lib/test-utils"
            "lib/text-class"
          ];
        }
        {
          src = inputs.ekg-forward;
          subdirs = [ "." ];
        }
        {
          src = inputs.ekg-json;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          src = inputs.hedgehog-extras;
          subdirs = [ "." ];
        }
        {
          src = inputs.hw-aeson;
          subdirs = [ "." ];
        }
        {
          src = inputs.hydra-poc;
          subdirs = [
            "hydra-cardano-api"
            "hydra-cluster"
            "hydra-node"
            "hydra-plutus"
            "hydra-prelude"
            "hydra-test-utils"
            "hydra-tui"
            "plutus-cbor"
            "plutus-merkle-tree"
          ];
        }
        {
          src = inputs.hysterical-screams;
          subdirs = [ "." ];
        }
        {
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "contra-tracer"
            "iohk-monitoring"
            "tracer-transformers"
            "plugins/backend-ekg"
            "plugins/backend-aggregation"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
            "plugins/scribe-systemd"
          ];
        }
        {
          src = inputs.io-sim;
          subdirs = [
            "io-classes"
            "io-sim"
            "strict-stm"
          ];
        }
        {
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
          src = inputs.ouroboros-network;
          subdirs = [
            "monoidal-synchronisation"
            "network-mux"
            "ntp-client"
            "ouroboros-consensus"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-protocol"
            "ouroboros-consensus-shelley"
            "ouroboros-network"
            "ouroboros-network-framework"
            "ouroboros-network-testing"
          ];
        }
        {
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "plutus-tx-plugin"
            "prettyprinter-configurable"
            "stubs/plutus-ghc-stub"
            "word-array"
          ];
        }
        {
          src = inputs.plutus-apps;
          subdirs = [
            # "doc"
            "freer-extras"
            # "playground-common"
            # "plutus-chain-index"
            "plutus-chain-index-core"
            "plutus-contract"
            "plutus-contract-certification"
            "plutus-ledger"
            "plutus-ledger-constraints"
            # "plutus-pab"
            # "plutus-playground-server"
            "plutus-script-utils"
            # "plutus-use-cases"
            "quickcheck-dynamic"
            # "web-ghc"
          ];
        }
        {
          src = inputs.typed-protocols;
          subdirs = [
            "typed-protocols"
            "typed-protocols-cborg"
            "typed-protocols-examples"
          ];
        }
        {
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      additional = ps: [
        ps.hydra-test-utils
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          inherit cabalProjectLocal extraSources;
          name = "hydra-demo";
          compiler-nix-name = "ghc8107";
          shell = {
            additional = ps: [
              ps.hydra-cardano-api
              ps.hydra-cluster
              ps.hydra-node
              ps.hydra-plutus
              ps.hydra-prelude
              ps.hydra-test-utils
              ps.hydra-tui
              # ps.plutus-contract
              ps.plutus-ledger-constraints
              ps.plutus-script-utils
            ];
            withHoogle = false;
            tools.haskell-language-server = { };
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              cabal-install
              haskellPackages.cabal-fmt
              haskellPackages.implicit-hie
              haskellPackages.fourmolu
              hlint
              jq
              websocat
              fd
              nixpkgs-fmt
            ];
          };
          modules = haskellModules;
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';

      flake = nixpkgsFor.hydraDemoProject.flake { };
      exe-component-name = "hydra-demo:exe:hydra-rps-game";

    in
    {
      inherit cabalProjectLocal extraSources haskellModules nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let exe = "hydra-demo:exe:hydra-rps-game";
        in self.flake.${system}.packages.${exe}
      );
      defaultApp = perSystem (system:
        let exe = "hydra-demo:exe:hydra-rps-game";
        in self.flake.${system}.apps.${exe}
      );

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");
      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });
    };
    # flake // {
    #   defaultPackage = flake.packages.${exe-component-name};
    #   defaultApp = flake.apps.${exe-component-name};
    #   check = nixpkgsFor.runCommand "combined-test"
    #     {
    #       nativeBuildInputs = builtins.attrValues flake.checks;
    #     } "touch $out";
    # };
}
