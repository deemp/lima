{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    devshell.url = "github:deemp/flakes?dir=devshell";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.lib.${system}) extensions settingsNix writeSettingsJSON mkCodium;
      inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommandsDir mkShell;
      inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
      inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
      inherit (inputs.workflows.lib.${system}) writeWorkflow steps nixCI_ run stepsIf names os;
      inherit (inputs.drv-tools.lib.${system}) mkShellApps mkBin;

      # Next, set the desired GHC version
      ghcVersion = "928";

      # and the name of the package
      myPackageName = "lima";

      override =
        {
          overrides = self: super: {
            myPackage = pkgs.haskell.lib.overrideCabal (super.callCabal2nix myPackageName ../. { }) (
              x: {
                testHaskellDepends = [
                  super.doctest-parallel_0_3_0_1
                ] ++ (x.testHaskellDepends or [ ]);
              }
            );
          };
        };

      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.myPackage ]);
      })
        cabal hpack ghcid hls;

      tools = [
        hls
        cabal
        hpack
        ghcid
      ];

      nix-dev = "nix-dev/";

      packages =
        let
          packages1 = mkShellApps {
            writeREADME = {
              text = "${mkBin cabal} test readme-hs-to-md";
            };
          };
          packages2 = {
            codium = mkCodium {
              extensions = { inherit (extensions) nix haskell misc github markdown; };
              runtimeDependencies = tools;
            };
            writeSettings = writeSettingsJSON {
              inherit (settingsNix) haskell todo-tree files editor gitlens yaml
                git nix-ide workbench markdown-all-in-one markdown-language-features;
            };
            inherit (mkFlakesTools [ "." nix-dev ]) updateLocks pushToCachix;
            writeWorkflows = writeWorkflow "CI" (
              (nixCI_ {
                dir = nix-dev;
                steps_ = dir: stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") [
                  steps.configGitAsGHActions
                  (steps.updateLocks { inherit dir; doCommit = false; })
                  {
                    name = "Convert README.hs to README.md";
                    run = run.nixScript {
                      inherit dir;
                      name = packages1.writeREADME.pname;
                    };
                  }
                  {
                    name = "Commit changes";
                    run = run.nix {
                      doCommit = true;
                      commitMessages = [ "Update flake locks" "Convert README.hs to README.md" ];
                    };
                  }
                ] ++ [
                  {
                    name = "Build lima";
                    run = run.nix {
                      dir = ".";
                      doRun = false;
                      scripts = [ "" "sdist" ];
                    };
                  }
                ];
              }) // {
                permissions = {
                  contents = "write";
                };
              }
            );
          };
        in
        packages1 // packages2;

      devShells.default = mkShell {
        packages = tools;
        bash.extra = "export LC_ALL=C.UTF-8";
        commands =
          mkCommands "tools" tools ++
          mkRunCommandsDir nix-dev "ide" {
            "codium ." = packages.codium;
            inherit (packages) writeSettings;
          } ++
          [
            {
              name = "cabal-test";
              category = "test";
              help = "Test via `cabal`";
              command = "cabal v1-test";
            }
          ];
      };
    in
    {
      inherit packages devShells;
    });
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
