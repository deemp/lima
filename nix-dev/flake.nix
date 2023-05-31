{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    devshell.url = "github:deemp/flakes?dir=devshell";
    workflows.url = "github:deemp/flakes?dir=workflows";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommandsDir mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.workflows.functions.${system}) writeWorkflow installNix cacheNixDirs stepsIf expr run;
      inherit (inputs.workflows.configs.${system}) nixCI steps names os strategies on env;

      # Next, set the desired GHC version
      ghcVersion = "927";

      # and the name of the package
      myPackageName = "lima";

      inherit (pkgs.haskell.lib)
        dontCheck
        dontStrip
        dontHaddock
        overrideCabal
        ;

      override =
        let donts = drv: pkgs.lib.trivial.pipe drv [ dontStrip dontCheck dontHaddock ]; in
        {
          overrides = self: super: {
            myPackage = overrideCabal (super.callCabal2nix myPackageName ../. { }) (
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

      packages = {
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens yaml
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };
        inherit (mkFlakesTools [ "." "../" ]) updateLocks pushToCachix;
        writeWorkflows =
          writeWorkflow "CI" {
            jobs = {
              nixCI = {
                name = "Nix CI";
                strategy = strategies.nixCache;
                runs-on = expr names.matrix.os;
                steps =
                  [
                    steps.checkout
                    (installNix { store = expr names.matrix.store; })
                  ]
                  ++
                  (stepsIf ("${names.matrix.os} == '${os.ubuntu-20}'") [
                    (cacheNixDirs { keySuffix = "cachix"; store = expr names.matrix.store; restoreOnly = false; })
                    steps.configGitAsGHActions
                    (
                      let name = "Update flake locks"; in
                      {
                        inherit name;
                        run = run.nixRunAndCommitDir nix-dev names.updateLocks name;
                      }
                    )
                  ])
                  ++ [
                    steps.logInToCachix
                    {
                      name = "Push flakes to Cachix";
                      env.CACHIX_CACHE = expr names.secrets.CACHIX_CACHE;
                      run = run.nixRunDir nix-dev names.pushToCachix;
                    }
                  ]
                ;
              };
            };
            name = "Nix CI";
            inherit on;
          };
      };

      devShells.default = mkShell {
        packages = tools;
        bash.extra = "export LC_ALL=C.UTF-8";
        commands =
          mkCommands "tools" tools ++
          mkRunCommandsDir "nix-dev" "ide" {
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
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
