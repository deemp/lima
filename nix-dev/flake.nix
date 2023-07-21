{
  inputs = { };
  outputs = inputs:
    let
      inputs_ =
        let
          root = import ../.;
          flakes = root.outputs.inputs.flakes;
        in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils formatter;
          inherit (flakes) drv-tools workflows flakes-tools devshell codium;
          haskell-tools = flakes.language-tools.haskell;
          inherit root;
        };

      outputs = outputs_ { } // { inherit outputs_; inputs = inputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in
        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix writeSettingsJSON mkCodium;
            inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommandsDir mkShell;
            inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
            inherit (inputs.workflows.lib.${system}) writeWorkflow steps nixCI run stepsIf names os;
            inherit (inputs.drv-tools.lib.${system}) mkShellApps getExe;
            inherit (inputs.root.params.${system}) override ghcVersion lima;

            # Next, set the desired GHC version
            inherit (toolsGHC {
              inherit override;
              version = ghcVersion;
              packages = ps: [ ps.${lima} ];
            }) cabal hls hpack fourmolu ghcid haskellPackages;

            tools = [ hls cabal hpack ghcid fourmolu ];

            nix-dev = "nix-dev/";

            packages =
              let
                packages1 = mkShellApps { writeDocs.text = "${getExe cabal} test ${lima}:test:readme"; };
                packages2 = {
                  codium = mkCodium { extensions = extensionsCommon // { inherit (extensions) haskell; }; };
                  writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
                  inherit (mkFlakesTools { dirs = [ "." nix-dev ]; root = ../.; }) updateLocks pushToCachix format;
                  writeWorkflows = writeWorkflow "CI"
                    (
                      nixCI {
                        jobArgs = {
                          dir = nix-dev;
                          doPushToCachix = true;
                          cacheNixArgs = {
                            linuxGCEnabled = true;
                            linuxMaxStoreSize = 5100000000;
                            macosGCEnabled = true;
                            macosMaxStoreSize = 5100000000;
                          };
                          doUpdateLocks = true;
                          doCommit = false;
                          doSaveFlakes = false;
                          steps = dir:
                            stepsIf ("${names.matrix.os} == '${os.ubuntu-22}'")
                              (
                                let writeDocsName = "Write docs"; in
                                [
                                  {
                                    name = writeDocsName;
                                    run = run.nixScript {
                                      inherit dir;
                                      name = packages1.writeDocs.pname;
                                    };
                                  }
                                  {
                                    name = "Commit & Push changes";
                                    run = run.nix {
                                      doCommit = true;
                                      commitArgs.messages = [ (steps.format { }).name (steps.updateLocks { }).name writeDocsName ];
                                    };
                                  }
                                ]
                              )
                            ++ [
                              {
                                name = "Build lima";
                                run = ''
                                  ${run.nixScript { name = ""; doRun = false; }}
                                  ${run.nixScript { name = "sdist"; doRun = false; installPriority = 1; }}
                                '';
                              }
                            ];
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
                mkCommands "tools" tools
                ++ mkRunCommandsDir nix-dev "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
                ++ [
                  {
                    name = "test";
                    category = "test";
                    help = "Build via `cabal`";
                    command = "cabal v1-test";
                  }
                ];
            };
          in
          {
            inherit packages devShells;
          })
        // { inherit (inputs) formatter; };
    in
    outputs;

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
