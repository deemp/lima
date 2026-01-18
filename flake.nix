{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell = {
      url = "github:deemp/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cache-nix-action = {
      url = "github:nix-community/cache-nix-action";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        let
          hpkgsFinal = config.haskellProjects.default.outputs.finalPackages;

          # Our only Haskell project. You can have multiple projects, but this template
          # has only one.
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # To avoid unnecessary rebuilds, we filter projectRoot:
            # https://community.flake.parts/haskell-flake/local#rebuild
            projectRoot = builtins.toString (
              lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./lima
                  ./cabal.project
                  ./README.md
                ];
              }
            );

            basePackages = pkgs.haskell.packages."ghc910";

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
              hoogle = false;
              tools = hp: {
                hlint = null;
                ghcid = null;
                cabal-install = null;
                haskell-language-server = null;
              };
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              shellcheck.enable = true;
              fourmolu.enable = true;
              prettier.enable = true;
            };
            settings = {
              excludes = [
                ".envrc"
              ];
              formatter = {
                fourmolu.excludes = [
                  "**/*.cabal"
                  "**/Setup.hs"
                  "**/testdata/**/*.hs"
                ];
              };
            };
          };

          legacyPackages = {
            saveFromGC = import "${inputs.cache-nix-action.outPath}/saveFromGC.nix" {
              inherit inputs pkgs;
            };
          };

          cabal = pkgs.cabal-install;

          packages = {
            default = hpkgsFinal.lima;
            lima-sdist = (hpkgsFinal.buildFromCabalSdist hpkgsFinal.lima).overrideAttrs (_: {
              pname = "lima-sdist";
            });
            writeDocs = pkgs.writeShellApplication {
              name = "writeDocs";
              runtimeInputs = [ cabal ];
              text = "cabal test lima:test:readme";
            };
          };

          # Default shell.
          devshells.default = {
            packagesFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];
            bash.extra = ''
              export LC_ALL=C.UTF-8
            '';
            commandGroups = {
              tools = [
                {
                  expose = true;

                  packages = {
                    inherit cabal;
                    inherit (pkgs) hpack;
                    inherit (hpkgsFinal) haskell-language-server;
                  };
                }
              ];

              packages = [
                {
                  prefix = "nix run .#";

                  packages = {
                    inherit (hpkgsFinal) lima;
                    inherit (packages) lima-sdist;
                  };
                }
              ];
            };
          };
        in
        {
          inherit
            haskellProjects
            treefmt
            legacyPackages
            packages
            devshells
            ;
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
